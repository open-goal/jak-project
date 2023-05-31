// TODO - convert this to a proper class

#include "third-party/json.hpp"

using json = nlohmann::json;

class InitializeResult {
 public:
  InitializeResult(){};
  json to_json() { return result; }

 private:
  json text_document_sync{
      {"openClose", true},
      {"change", 1},  // Full sync
      {"willSave", false},
      {"willSaveWaitUntil", false},
      {"save", {{"includeText", false}}},
  };

  json completion_provider{
      {"resolveProvider", false},
      {"triggerCharacters", {}},
  };
  json signature_help_provider{{"triggerCharacters", ""}};
  json code_lens_provider{{"resolveProvider", false}};
  json document_on_type_formatting_provider{
      {"firstTriggerCharacter", ""},
      {"moreTriggerCharacter", ""},
  };
  json document_link_provider{{"resolveProvider", false}};
  json execute_command_provider{{"commands", {}}};

  json document_symbol_provder{{"label", "OpenGOAL"}};

  json result{{"capabilities",
               {
                   {"textDocumentSync", text_document_sync},
                   {"hoverProvider", true},
                   {"completionProvider", completion_provider},
                   {"signatureHelpProvider", signature_help_provider},
                   {"definitionProvider", true},
                   {"colorProvider", true},
                   {"referencesProvider", false},
                   {"documentHighlightProvider", false},
                   {"documentSymbolProvider",
                    document_symbol_provder},  // TODO - there is another selectionRangeProvider i
                                               // think i need, or word boundaries need to change!
                   {"workspaceSymbolProvider", false},
                   {"codeActionProvider", false},
                   {"codeLensProvider", code_lens_provider},
                   {"documentFormattingProvider", true},
                   {"documentRangeFormattingProvider", false},
                   {"documentOnTypeFormattingProvider", document_on_type_formatting_provider},
                   {"renameProvider", false},
                   {"documentLinkProvider", document_link_provider},
                   {"executeCommandProvider", execute_command_provider},
                   {"experimental", {}},
               }}};
};
