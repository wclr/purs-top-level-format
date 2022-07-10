module Extension (activate) where


import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import VSCode (Document, Range, TextEdit)
import VSCode.Context (ExtensionContext, addSubscription)
import VSCode.Languages (DocumentSelector, FormattingOptions, registerDocumentFormattingEditProvider, registerDocumentRangeFormattingEditProvider)
import VSCode.Window (appendLine, createOutputChannel)


topFormat :: Document -> Aff (Array TextEdit)
topFormat x = pure []


rangeFormat :: Document -> Range -> Aff (Array TextEdit)
rangeFormat x range = pure []


activate :: ExtensionContext -> Effect Unit
activate context = do
  output <- createOutputChannel "Purs top level format real"
  appendLine output "Hello"
  addSub
    =<< registerDocumentFormattingEditProvider
        sel
        (\doc _ -> topFormat doc)
  addSub
    =<< registerDocumentRangeFormattingEditProvider
        sel
        (\doc range _ -> rangeFormat doc range)
  where
  addSub = addSubscription context
  sel = { scheme: "file", language : "purescript"}


main :: Effect Unit
main = do
  log "🍝"
