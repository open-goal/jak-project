#pragma once

#include <functional>
#include <optional>
#include <string>
#include <vector>

std::vector<std::optional<std::string>> isolated_map(
    size_t count,
    int jobs,
    size_t chunk_size,
    const std::function<std::string(size_t)>& task,
    const std::function<void()>& on_done = nullptr);
