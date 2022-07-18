#include "document_diagnostics.h"

void LSPSpec::to_json(json& j, const CodeDescription& obj) {
  j = json{{"href", obj.m_href}};
}

void LSPSpec::from_json(const json& j, CodeDescription& obj) {
  j.at("href").get_to(obj.m_href);
}

void LSPSpec::to_json(json& j, const DiangosticRelatedInformation& obj) {
  j = json{{"location", obj.m_location}, {"message", obj.m_message}};
}

void LSPSpec::from_json(const json& j, DiangosticRelatedInformation& obj) {
  j.at("location").get_to(obj.m_location);
  j.at("message").get_to(obj.m_message);
}

void LSPSpec::to_json(json& j, const Diagnostic& obj) {
  j = json{{"range", obj.m_range}, {"severity", obj.m_severity}, {"message", obj.m_message}};
  if (obj.m_code) {
    j["code"] = obj.m_code.value();
  }
  if (obj.m_codeDescription) {
    j["codeDescription"] = obj.m_codeDescription.value();
  }
  if (obj.m_source) {
    j["source"] = obj.m_source.value();
  }
  if (obj.m_tags) {
    j["tags"] = obj.m_tags.value();
  }
  if (obj.m_relatedInformation) {
    j["relatedInformation"] = obj.m_relatedInformation.value();
  }
}

void LSPSpec::from_json(const json& j, Diagnostic& obj) {
  j.at("range").get_to(obj.m_range);
  j.at("severity").get_to(obj.m_severity);
  j.at("message").get_to(obj.m_message);
  if (j.contains("code")) {
    obj.m_code = std::make_optional(j.at("code").get<std::string>());
  }
  if (j.contains("codeDescription")) {
    obj.m_codeDescription = std::make_optional(j.at("codeDescription").get<CodeDescription>());
  }
  if (j.contains("source")) {
    obj.m_source = std::make_optional(j.at("source").get<std::string>());
  }
  if (j.contains("tags")) {
    obj.m_tags = std::make_optional(j.at("tags").get<std::vector<DiagnosticTag>>());
  }
  if (j.contains("relatedInformation")) {
    obj.m_relatedInformation = std::make_optional(
        j.at("relatedInformation").get<std::vector<DiangosticRelatedInformation>>());
  }
}

void LSPSpec::to_json(json& j, const PublishDiagnosticParams& obj) {
  j = json{{"uri", obj.m_uri}, {"diagnostics", obj.m_diagnostics}};
  if (obj.m_version) {
    j["version"] = obj.m_version.value();
  }
}

void LSPSpec::from_json(const json& j, PublishDiagnosticParams& obj) {
  j.at("uri").get_to(obj.m_uri);
  j.at("diagnostics").get_to(obj.m_diagnostics);
  if (j.contains("version")) {
    obj.m_version = std::make_optional(j.at("version").get<int32_t>());
  }
}
