#include "elzip/elzip.hpp"

#include <gtest/gtest.h>
#include <fstream>

static const std::filesystem::path locAssetDir{ASSETS_DIR "_Live"};

class UnzipFileTestFixture : public ::testing::Test
{
protected:
    void SetUp() override
    {
        std::filesystem::copy(ASSETS_DIR, locAssetDir, std::filesystem::copy_options::overwrite_existing | std::filesystem::copy_options::recursive);
    }

    void TearDown() override
    {
        if (HasFailure())
        {
            std::cout << "===== ===== assetContent ===== =====\n";
            for (const auto& it : std::filesystem::recursive_directory_iterator(std::filesystem::path(locAssetDir)))
            {
                std::cout << " > " << it.path().u8string() << '\n';
#if also_prints_content_of_binary_files
                if (it.is_regular_file())
                {
                    auto file = std::ifstream(it.path());
                    std::cout << std::string((std::istreambuf_iterator<char>(file)), (std::istreambuf_iterator<char>())) << '\n';
                    file.close();
                }
#endif
            }
        }
        std::filesystem::remove_all(locAssetDir);
    }
};

TEST_F(UnzipFileTestFixture, can_unzip_an_archive_which_contains_binary_files)  // NOLINT
{
    EXPECT_EXIT(
        {
            elz::extractZip(locAssetDir / "patch.zip", locAssetDir / "patch");
            exit(0);
        },
        ::testing::ExitedWithCode(0),
        "");
}

TEST_F(UnzipFileTestFixture, files_are_actualy_created_while_unzipping)  // NOLINT
{
    const auto patchDir = locAssetDir / "patch";
    elz::extractZip(locAssetDir / "patch.zip", patchDir);
    EXPECT_TRUE(std::filesystem::exists(patchDir / "test.txt"));
    EXPECT_TRUE(std::filesystem::exists(patchDir / EXE_NAME));
}

bool CompareStreams(std::ifstream&& lhs, std::ifstream&& rhs)
{
    while (lhs.good() && rhs.good())
    {
        if (lhs.get() != rhs.get())
        {
            return false;
        }
    }
    return lhs.eof() == rhs.eof();
}

TEST_F(UnzipFileTestFixture, unziped_files_are_equivalent_to_original)  // NOLINT
{
    const auto patchDir = locAssetDir / "patch";
    elz::extractZip(locAssetDir / "patch.zip", patchDir);
    EXPECT_TRUE(CompareStreams(std::ifstream(locAssetDir / "test.txt"), std::ifstream(patchDir / "test.txt")));
    EXPECT_TRUE(CompareStreams(std::ifstream(locAssetDir / EXE_NAME), std::ifstream(patchDir / EXE_NAME)));
}

TEST(unzipping, support_empty_files)  // NOLINT
{
    std::filesystem::path basePath = "folder_with_empty_file";
    std::filesystem::create_directory(basePath);
    std::ofstream emptyFile(basePath / "empty_file");
    elz::zipFolder(basePath, "test.zip");
    std::filesystem::path outputPath = "folder_with_empty_file_out";
    std::filesystem::create_directory(outputPath);
    elz::extractZip("test.zip", outputPath);
    EXPECT_TRUE(CompareStreams(std::ifstream(basePath / "empty_file"), std::ifstream(outputPath / "empty_file")));
}