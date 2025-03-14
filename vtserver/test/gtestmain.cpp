/**
 * Copyright @ 2020 - 2020 iAUTO(Shanghai) Co., Ltd.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are NOT permitted except as agreed by
 * iAUTO(Shanghai) Co., Ltd.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */

#include "gtest/gtest.h"
#include "Environment.h"

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);

    AddGlobalTestEnvironment(new Environment());

    return RUN_ALL_TESTS();
}
/* EOF */
