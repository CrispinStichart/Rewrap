/// A source code document is one that has source code and comments. Source code is only
/// wrapped if selected expressly.
module internal Parsing_SourceCode

// The main function is `sourceCode`. That, `lineComment` and `blockComment` are the
// public functions.
//
// Lots of code from Parsing.Comments has been copy-pasted just to make it work.
// Refactoring will come later

open Prelude
open Line
open Parsing_
open Parsing_Internal
open System
open System.Text.RegularExpressions


/// Decoration lines are not wrapped and their prefix is not modified. With normal lines
/// the prefix may be adjusted if reformat is on, and the content is passed to the content
/// parser.
type DecorationLine(basedOn: Line) = inherit Line (basedOn.prefix, basedOn.content)


/// Convert all tabs in a line's content to spaces, to make things easier. (Markdown, the
/// content processor used in most cases, doesn't allow them anyway.)
let private tabsToSpacesContent : int -> Line -> Line =
  fun tabSize ->
  let step (maybeAccStr, accWidth) s =
    let accWidth' = accWidth + strWidth tabSize s
    match maybeAccStr with
    | None -> Some s, accWidth'
    | Some accStr ->
        let spcCount = tabSize - (accWidth % tabSize)
        Some (accStr + String.replicate spcCount " " + s), accWidth' + spcCount
  let convert initWidth (str: string) =
    str.Split([|'\t'|]) |> Array.fold step (None, initWidth) |> fst |> Option.get

  fun line -> Line.mapContent (convert (strWidth tabSize line.prefix)) line


let private splitAtWidth : int -> int -> int -> Line -> Line =
  fun tabWidth leftWidth extraWidth line ->
  // This will turn a double-width char into spaces if split within it
  let spaces n = String.replicate n " "
  let rec loop accWidth p =
    if p >= line.content.Length then Line(line.prefix + line.content, "") else

    let cc = (uint16) line.content.[p]
    let ccWidth = charWidth tabWidth (leftWidth + accWidth) cc
    let diff = extraWidth - accWidth - ccWidth

    if diff = 0 then Line.adjustSplit (p+1) line
    elif diff > 0 then loop (accWidth + ccWidth) (p+1)
    else // This is designed for tabs going over the split point. Are replaced with spaces
      let line = Line.adjustSplit p line
      Line(line.prefix + spaces (diff + ccWidth), spaces -diff + line.content.Substring(1))
  (if extraWidth < 1 then line else loop 0 0) |> tabsToSpacesContent tabWidth

/// Regex that captures whitespace at the beginning of a string
let private wsRegex = regex @"^\s*"

let private isCStyleBlockComment (startMarker: string, endMarker: string) =
  startMarker.Contains(@"/\*") && endMarker.Contains(@"\*/")

type BlockCommentInfo =
  { bodyMarkers: string
    defaultBodyMarker: string
    prefixStart: int
    prefixLen: int
    baseIndent: string
    isCStyle: bool
    alignEndMarkerToBody: bool }

type CommentFormat =
  /// Line comment (//). Takes the comment lines and the column that's immediately after
  /// the comment markers.
  | LineFmt of Line Nonempty * int
  /// Block comment (/* ... */) that has multiple lines. Takes the first line with type; a
  /// list of "body" lines; the last line separately, if it only contains the end comment
  /// marker (without text preceeding it); and a string of characters that are allowed in
  /// each line prefix (eg "*")
  | MultiLineBlockFmt of Line * Line List * Line Option * BlockCommentInfo
  /// Block comment, but one that's only on a single line. Takes the prefix to use if new
  /// lines are added.
  | SingleLineBlockFmt of Line * BlockCommentInfo


/// Wrapper that makes sure DecorationLines aren't wrapped
let withDecorations : ContentParser -> PrefixTransformer -> ContentParser =
  fun contentParser prefixFn ctx ->

  let rec wrapFLR = function
    | Pending r -> Pending (wrapResultParser nlpWrapper r)
    | Finished r -> Finished (wrapResultParser (fun p -> Some (flpWrapper p)) r)

  and flpWrapper maybeFLP : FirstLineParser = function
    | :? DecorationLine as line -> finished line noWrapBlock (flpWrapper None)
    | _ as line -> (maybeFLP |? contentParser ctx) line |> wrapFLR

  and nlpWrapper nlp : NextLineParser = function
    | :? DecorationLine as line -> FinishedOnPrev <| Some (flpWrapper None line)
    | _ as line -> nlp line |> function
        | ThisLine r -> ThisLine (wrapFLR r)
        | FinishedOnPrev maybeR ->
            FinishedOnPrev <| Some (maybe' (fun _ -> flpWrapper None line) wrapFLR maybeR)

  contentParser ctx >> wrapFLR >> wrapPrefixFn prefixFn


let private mkPrefixFn start len rep (pre: string) : string =
  pre.Substring(0, start) + rep + pre.Substring(start + len)


/// Takes comment lines from either a line or block comment and parses into blocks.
let private inspectAndProcessContent :
   ContentParser -> CommentFormat -> Line option -> Line option -> Context -> unit =
  fun contentParser fmt extraStartLine extraEndLine ctx ->

  let tabWidth = ctx.settings.tabWidth

  // Depending on the type of comment block, get the lines we want to look at, prefix
  // regex and initial indent (indent up to inc comment marker)
  let (lines: Line seq), prefixRegex, initialIndent =
    match fmt with
    | LineFmt (lines, initialIndent) ->
        lines :> seq<Line>, wsRegex, initialIndent
    | MultiLineBlockFmt (_, tLines, _, info) ->
        let bm = if info.bodyMarkers <> "" then "[" + info.bodyMarkers + @"]?\s*" else ""
        tLines :> seq<Line>, regex (@"^\s*" + bm), 0
    | SingleLineBlockFmt (_, info) ->
        Seq.empty, wsRegex, 0

  let strWidth = strWidth' initialIndent tabWidth

  // 1st pass: Examine each line's content after the prefix. Non-blank, non-text lines are
  // all marked as DecorationLines here. Otherwise, if the line is not blank, note the
  // indent of the content. The minimum indent of all normal lines will be taken as the
  // content indent for the whole block. We get back lines with decoration lines marked,
  // and the amount we need to increase prefix for all lines by.
  let lines, indentIncrease =
    let mapping minIndent (line: Line) =
      let m = prefixRegex.Match(line.content)
      if line.content.Length = m.Length then line, minIndent
      else if (containsText line.content) then line, min minIndent (strWidth m.Value)
      else DecorationLine(line) :> Line, minIndent
    lines |> Seq.mapFold mapping Int32.MaxValue

  // 2nd pass: examine existing decoration lines, and if their content is indented the
  // same as or further than the body indent, then convert them back to normal content
  // lines. (In the case of a block comment, this is not done to the first or last line.)
  // For each normal line, increment its prefix so it matches that of the body indent,
  // then convert post-prefix tabs to spaces. The bodyPrefix is the prefix to use for
  // newly-lines created after the first
  let lines, maybeBodyPrefix =
    let rec adjust : Line -> Line * Option<string> = function
      | :? DecorationLine as line ->
          let m = prefixRegex.Match(line.content)
          if strWidth m.Value >= indentIncrease then adjust (Line line) else upcast line, None
      | _ as line ->
          let line = splitAtWidth tabWidth initialIndent indentIncrease line
                      |> tabsToSpacesContent tabWidth
          line, if Line.isBlank line.content then None else Some line.prefix
    let mapping maybePrefix = adjust >> rmap (fun mlp -> maybePrefix <|> mlp)
    lines |> Seq.mapFold mapping None

  let defaultPrefix =
    match fmt with
      | LineFmt _ -> ""
      | MultiLineBlockFmt (_, _, _, info) -> info.baseIndent + info.defaultBodyMarker
      | SingleLineBlockFmt (_, info) -> info.baseIndent + info.defaultBodyMarker

  let bodyPrefix = maybeBodyPrefix |? defaultPrefix

  let desiredBodyPrefix =
    match fmt with
      | LineFmt _ -> bodyPrefix
      | MultiLineBlockFmt (line, _, _, info)
      | SingleLineBlockFmt (line, info) ->
          let addStars = ctx.settings.blockCommentAddAsterisks
          let align = ctx.settings.blockCommentAlignWithFirstLine
          if not info.isCStyle || (not addStars && not align) then bodyPrefix
          else
            let marker = if addStars then " * " else ""
            let markerWidth = strWidth marker
            let baseIndent =
              if align then
                if extraStartLine.IsSome || line :? DecorationLine then leadingWhitespace bodyPrefix
                else
                  let target = strWidth line.prefix
                  let indentWidth = max 0 (target - markerWidth)
                  String.replicate indentWidth " "
              else leadingWhitespace bodyPrefix
            baseIndent + marker

  let adjustEndLine (line: Line) =
    let indent = leadingWhitespace desiredBodyPrefix
    let content = line.content.TrimStart()
    Line("", indent + content)

  let lines, prefixFn =
    match fmt with
      | LineFmt _ -> Nonempty.fromSeqUnsafe lines, id
      | MultiLineBlockFmt (line, _, mbLastLine, info) ->
        let last: seq<Line> =
          maybe Seq.empty (fun l ->
            let l = if info.alignEndMarkerToBody then adjustEndLine l else l
            Seq.singleton (upcast DecorationLine(l))) mbLastLine
        let lines = line .@ List.ofSeq (Seq.append lines last)
        let prefixFn = mkPrefixFn info.prefixStart info.prefixLen desiredBodyPrefix
        lines, prefixFn
      | SingleLineBlockFmt (line, info) ->
          let prefixFn = mkPrefixFn info.prefixStart info.prefixLen desiredBodyPrefix
          singleton line, prefixFn

  let lines =
    match fmt with
      | MultiLineBlockFmt _
      | SingleLineBlockFmt _ when extraStartLine.IsSome ->
          let (Nonempty(first, rest)) = lines
          let first = first |> Line.mapPrefix (fun _ -> desiredBodyPrefix)
          Nonempty(first, rest)
      | _ -> lines

  let lines =
    match fmt with
      | MultiLineBlockFmt (_, _, _, info)
      | SingleLineBlockFmt (_, info) ->
          if info.isCStyle && (ctx.settings.blockCommentAddAsterisks || ctx.settings.blockCommentAlignWithFirstLine) then
            let (Nonempty(first, rest)) = lines
            let rest =
              rest
              |> List.map (fun line ->
                  if line :? DecorationLine then line
                  else line |> Line.mapPrefix (fun _ -> desiredBodyPrefix))
            Nonempty(first, rest)
          else lines
      | LineFmt _ -> lines

  match extraStartLine with
  | Some line -> ctx.addBlock (ExtraLine (line.prefix + line.content))
  | None -> ()

  processContent (withDecorations contentParser prefixFn) ctx lines

  match extraEndLine with
  | Some line ->
      let line =
        match fmt with
        | MultiLineBlockFmt (_, _, _, info)
        | SingleLineBlockFmt (_, info) ->
            if info.alignEndMarkerToBody then adjustEndLine line else line
        | LineFmt _ -> line
      ctx.addBlock (ExtraLine (line.prefix + line.content))
  | None -> ()



type private LineCommentBlock (contentParser: ContentParser) =
  inherit NewBlock (BlockType.Comment)
  // To start with this just adds blocks to the container We'll need a prefixFn if we
  // support end-of-line comments
  override _.output ctx lines =
    let fmt = LineFmt (lines, strWidth ctx.settings.tabWidth (Nonempty.head lines).prefix)
    inspectAndProcessContent contentParser fmt None None ctx


let lineComment : string -> ContentParser -> TryNewParser =
  fun marker contentParser ->
  let rx = regex (@"^(\s*)" + marker)
  let tryMatchLine line = tryMatch rx line <|>> fun m -> Line.adjustSplit m.[0].Length line
  let comment = LineCommentBlock (contentParser)

  fun ctx ->
  let strWidth = strWidth' 0 ctx.settings.tabWidth

  let rec readRestOfBlock blockPrefixWidth =
    let rec readLine line =
      match tryMatchLine line with
      | Some line ->
         let linePrefixWidth = strWidth line.prefix
         if linePrefixWidth = blockPrefixWidth
           then ThisLine <| pending line comment readLine
           else FinishedOnPrev <| Some (pending line comment (readRestOfBlock linePrefixWidth))
      | None -> FinishedOnPrev None
    readLine

  tryMatchLine >> map (fun l -> pending l comment (readRestOfBlock (strWidth l.prefix)))


/// Takes the contents of the last line so that we later know when we've hit it
type private BlockCommentBlock
  (contentParser: ContentParser, info: BlockCommentInfo, mEndIndex: int, mEndLen: int) =
  inherit NewBlock (BlockType.Comment)
  override _.output ctx (Nonempty(hLine, tLines)) =

    let inline hasTextUpTo p (str: string) = Line.containsText (str.Substring(0, min str.Length p))
    let inline hasNonWhitespaceAfter p (str: string) =
      let after = str.Substring(p)
      not (String.IsNullOrWhiteSpace(after))

    let openOnNewLine = info.isCStyle && ctx.settings.blockCommentOpenOnNewLine
    let closeOnNewLine = info.isCStyle && ctx.settings.blockCommentCloseOnNewLine
    let inline validEnd (line: Line) =
      mEndLen > 0 && mEndIndex >= 0 && mEndIndex + mEndLen <= line.content.Length

    let extraStartLine =
      if openOnNewLine && Line.containsText hLine.content then
        let startLine = hLine.prefix.TrimEnd()
        Some (Line(startLine, startLine.Length))
      else None

    let mkFirstLine (line: Line) p : Line =
      if not (hasTextUpTo p line.content) then upcast (DecorationLine line)
      else tabsToSpacesContent ctx.settings.tabWidth line

    let fmt, extraEndLine =
      match Nonempty.fromList tLines with
      | None ->
          if closeOnNewLine && validEnd hLine
             && hasTextUpTo mEndIndex hLine.content
             && not (hasNonWhitespaceAfter (mEndIndex + mEndLen) hLine.content) then
            let truncated = Line(hLine.prefix, hLine.content.Substring(0, mEndIndex).TrimEnd())
            let indent = leadingWhitespace hLine.prefix
            let endLine = Line("", indent + hLine.content.Substring(mEndIndex, mEndLen))
            let fl = mkFirstLine truncated truncated.content.Length
            let info = { info with alignEndMarkerToBody = true }
            SingleLineBlockFmt (fl, info), Some endLine
          else
            SingleLineBlockFmt (mkFirstLine hLine mEndIndex, info), None
      | Some tail ->
          let bodyLines, nonTextLastLine, alignEndToBody, extraEndLine =
            let (Nonempty(lastLine, bodyRev)) = Nonempty.rev tail
            if closeOnNewLine && validEnd lastLine
               && hasTextUpTo mEndIndex lastLine.content
               && not (hasNonWhitespaceAfter (mEndIndex + mEndLen) lastLine.content) then
              let before = lastLine.content.Substring(0, mEndIndex).TrimEnd()
              let indent = leadingWhitespace lastLine.prefix
              let bodyLine = Line(lastLine.prefix, before)
              let endLine = Line("", indent + lastLine.content.Substring(mEndIndex, mEndLen))
              List.rev (bodyLine :: bodyRev), None, true, Some endLine
            elif hasTextUpTo mEndIndex lastLine.content then tLines, None, false, None
            else List.rev bodyRev, Some lastLine, false, None
          let fl = mkFirstLine hLine hLine.content.Length
          let info = { info with alignEndMarkerToBody = alignEndToBody }
          MultiLineBlockFmt (fl, bodyLines, nonTextLastLine, info), extraEndLine

    inspectAndProcessContent contentParser fmt extraStartLine extraEndLine ctx

/// Block comment parser
/// Block comment parser
let blockComment :
  (string * string) -> (string * string) -> ContentParser -> TryNewParser =

  fun (bodyMarkers, defaultBodyMarker) (startMarker, endMarker) contentParser ->
  let startRegex = regex (@"^\s*" + startMarker + @"\s*")
  let isCStyle = isCStyleBlockComment (startMarker, endMarker)
  let comment prefixStart prefixLen baseIndent endMatchIndex endMatchLen =
    let info =
      { bodyMarkers = bodyMarkers
        defaultBodyMarker = defaultBodyMarker
        prefixStart = prefixStart
        prefixLen = prefixLen
        baseIndent = baseIndent
        isCStyle = isCStyle
        alignEndMarkerToBody = false }
    BlockCommentBlock(contentParser, info, endMatchIndex, endMatchLen)

  let onFindStart (startLine: Line) (m: string[]) : FirstLineRes =
    // Make prefix replacement fn
    let pre =
      if m.Length = 1 || m.[1] = "" then m.[0]
      else m.[0].Replace(m.[1], String.replicate m.[1].Length " ")
    let baseIndent = leadingWhitespace pre
    let prefixStart = startLine.prefix.Length
    let prefixLen = m.[0].Length

    let defComment = comment prefixStart prefixLen baseIndent Int32.MaxValue 0
    let endPattern =
      let step (i:int, r:string) s = i + 1, r.Replace("$" + i.ToString(), s)
      Array.fold step (0, endMarker) m |> snd
    let endRegex = regex endPattern

    let rec testForEnd (line: Line) : FirstLineRes =
      let m = endRegex.Match(line.content)
      if m.Success then finished_ line (comment prefixStart prefixLen baseIndent m.Index m.Length)
      else pending line defComment (ThisLine << testForEnd)

    let startLine = Line.adjustSplit m.[0].Length startLine
    testForEnd startLine

  fun _ctx line -> tryMatch startRegex line <|>> onFindStart line



let sourceCode : List<TryNewParser> -> DocumentProcessor =
  fun commentParsers ->
  let contentParser = (tryMany commentParsers |? fun _ line -> finished_ line noWrapBlock)
  docOf contentParser
