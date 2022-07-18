#pragma once

#include "common_types.h"

namespace LSPSpec {
enum class DiagnosticSeverity {
  /// Reports an error.
  Error = 1,
  /// Reports a warning.
  Warning = 2,
  /// Reports an information.
  Information = 3,
  /// Reports a hint.
  Hint = 4,
};

/// @brief Structure to capture a description for an error code.
struct CodeDescription {
  /// @brief An URI to open with more information about the diagnostic error.
  URI m_href;
};

void to_json(json& j, const CodeDescription& obj);
void from_json(const json& j, CodeDescription& obj);

enum class DiagnosticTag {
  /// @brief Unused or unnecessary code.
  /// Clients are allowed to render diagnostics with this tag faded out instead of having an error
  /// squiggle.
  Unnecessary = 1,
  /// @brief Deprecated or obsolete code.
  /// Clients are allowed to rendered diagnostics with this tag strike through.
  Deprecated = 2
};

struct DiangosticRelatedInformation {
  /// @brief The location of this related diagnostic information.
  Location m_location;
  /// @brief The message of this related diagnostic information.
  std::string m_message;
};

void to_json(json& j, const DiangosticRelatedInformation& obj);
void from_json(const json& j, DiangosticRelatedInformation& obj);

struct Diagnostic {
  /// @brief The range at which the message applies.
  Range m_range;
  /// @brief The diagnostic's severity. Can be omitted. If omitted it is up to the client to
  /// interpret diagnostics as error, warning, info or hint.
  DiagnosticSeverity m_severity;
  /// @brief The diagnostic's code, which might appear in the user interface.
  std::optional<std::string> m_code;
  /// @brief An optional property to describe the error code.
  std::optional<CodeDescription> m_codeDescription;
  /// @brief A human-readable string describing the source of this diagnostic, e.g. 'typescript' or
  /// 'super lint'.
  std::optional<std::string> m_source;
  /// @brief The diagnostic's message.
  std::string m_message;
  /// @brief Additional metadata about the diagnostic.
  std::optional<std::vector<DiagnosticTag>> m_tags;
  /// @brief An array of related diagnostic information, e.g. when symbol-names within a scope
  /// collide all definitions can be marked via this property.
  std::optional<std::vector<DiangosticRelatedInformation>> m_relatedInformation;
  // omitting `data` field
  /// A data entry field that is preserved between a `textDocument/publishDiagnostics` notification
  /// and `textDocument/codeAction` request
};

void to_json(json& j, const Diagnostic& obj);
void from_json(const json& j, Diagnostic& obj);

struct PublishDiagnosticParams {
  /// @brief The URI for which diagnostic information is reported
  DocumentUri m_uri;
  /// @brief Optional the version number of the document the diagnostics are published for
  std::optional<int32_t> m_version;
  /// @brief An array of diagnostic information items
  std::vector<Diagnostic> m_diagnostics;
};

void to_json(json& j, const PublishDiagnosticParams& obj);
void from_json(const json& j, PublishDiagnosticParams& obj);

}  // namespace LSPSpec
