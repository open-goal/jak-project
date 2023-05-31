#include "rule_config.h"

namespace formatter {

const std::unordered_map<std::string, std::vector<std::shared_ptr<FormattingRule>>> opengoal_rules =
    {{"defun", {std::make_shared<InnerFormattingRule>(0)}}};
}
