#include "rule_config.h"

namespace formatter {

// TODO - these are eventually going to need a refactor too, my depth is based on the true
// tree-depth, in reality we want it to be relative to the forms defined here because imagine
// multiple `let`s in the same form, their depths will be different but you want them relatively
// formatted the same

// TODO - populate these more

const std::unordered_map<std::string, std::vector<std::shared_ptr<FormattingRule>>> opengoal_rules =
    {{"defun", {std::make_shared<InnerFormattingRule>(1)}}};
}  // namespace formatter
