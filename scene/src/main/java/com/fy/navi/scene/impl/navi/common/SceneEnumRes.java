package com.fy.navi.scene.impl.navi.common;

import com.fy.navi.scene.R;

public class SceneEnumRes {

    public static AutoUIDrawable getDrawableEnumName(SceneCommonStruct.TbtIconAction value) {
        return switch (value) {
            case ManeuverIconTurnLeft -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou2_day);
            case ManeuverIconTurnRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou3_day);
            case ManeuverIconSlightLeft -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou4_day);
            case ManeuverIconSlightRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou5_day);
            case ManeuverIconTurnHardLeft -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou6_day);
            case ManeuverIconTurnHardRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou7_day);
            case ManeuverIconUTurn -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou8_day);
            case ManeuverIconContinue -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou9_day);
            case ManeuverIconWay -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou10_day);
            case ManeuverIconEntryRing -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou11_day);
            case ManeuverIconLeaveRing -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou12_day);
            case ManeuverIconSAPA -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou13_day);
            case ManeuverIconTollGate -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou14_day);
            case ManeuverIconDestination -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou15_day);
            case ManeuverIconTunnel -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou16_day);
            case ManeuverIconEntryLeftRing -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou17_day);
            case ManeuverIconLeaveLeftRing -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou18_day);
            case ManeuverIconUTurnRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou19_day);
            case ManeuverIconSpecialContinue ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou20_day);
            case ManeuverIconEntryRingLeft -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou21_day);
            case ManeuverIconEntryRingRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou22_day);
            case ManeuverIconEntryRingContinue ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou23_day);
            case ManeuverIconEntryRingUTurn -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou24_day);
            case ManeuverIconEntryLeftRingLeft ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou25_day);
            case ManeuverIconEntryLeftRingRight ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou26_day);
            case ManeuverIconEntryLeftRingContinue ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou27_day);
            case ManeuverIconEntryLeftRingUTurn ->
                    new AutoUIDrawable(R.drawable.img_tbt_hud_sou28_day);
            case ManeuverIconEntryRing1 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou50_day);
            case ManeuverIconEntryRing2 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou51_day);
            case ManeuverIconEntryRing3 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou52_day);
            case ManeuverIconEntryRing4 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou53_day);
            case ManeuverIconEntryRing5 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou54_day);
            case ManeuverIconEntryRing6 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou55_day);
            case ManeuverIconEntryRing7 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou56_day);
            case ManeuverIconEntryRing8 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou57_day);
            case ManeuverIconEntryRing9 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou58_day);
            case ManeuverIconEntryRing10 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou59_day);
            case ManeuverIconLeaveRing1 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou60_day);
            case ManeuverIconLeaveRing2 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou61_day);
            case ManeuverIconLeaveRing3 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou62_day);
            case ManeuverIconLeaveRing4 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou63_day);
            case ManeuverIconLeaveRing5 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou64_day);
            case ManeuverIconLeaveRing6 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou65_day);
            case ManeuverIconLeaveRing7 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou66_day);
            case ManeuverIconLeaveRing8 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou67_day);
            case ManeuverIconLeaveRing9 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou68_day);
            case ManeuverIconLeaveRing10 -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou69_day);
            case ManeuverIconMergeLeft -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou71_day);
            case ManeuverIconMergeRight -> new AutoUIDrawable(R.drawable.img_tbt_hud_sou70_day);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    public static AutoUIDrawable getDrawableEnumName(SceneCommonStruct.TbtExitIconAction value) {
        return switch (value) {
            case ManeuverIconTurnLeft -> new AutoUIDrawable(R.drawable.img_tbt_exit_hud_sou2);
            case ManeuverIconTurnRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou3);
            case ManeuverIconSlightLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou4);
            case ManeuverIconSlightRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou5);
            case ManeuverIconTurnHardLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou6);
            case ManeuverIconTurnHardRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou7);
            case ManeuverIconUTurn -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou8);
            case ManeuverIconContinue -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou9);
            case ManeuverIconWay -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou10);
            case ManeuverIconEntryRing -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou11);
            case ManeuverIconLeaveRing -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou12);
            case ManeuverIconSAPA -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou13);
            case ManeuverIconTollGate -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou14);
            case ManeuverIconDestination -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou15);
            case ManeuverIconTunnel -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou16);
            case ManeuverIconEntryLeftRing -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou17);
            case ManeuverIconLeaveLeftRing -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou18);
            case ManeuverIconUTurnRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou19);
            case ManeuverIconSpecialContinue -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou20);
            case fManeuverIconEntryRingLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou21);
            case ManeuverIconEntryRingRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou22);
            case ManeuverIconEntryRingContinue -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou23);
            case ManeuverIconEntryRingUTurn -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou24);
            case ManeuverIconEntryLeftRingLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou25);
            case fManeuverIconEntryLeftRingRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou26);
            case ManeuverIconEntryLeftRingContinue -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou27);
            case ManeuverIconEntryLeftRingUTurn -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou28);
            case ManeuverIconEntryRing1 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou50);
            case ManeuverIconEntryRing2 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou51);
            case ManeuverIconEntryRing3 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou52);
            case ManeuverIconEntryRing4 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou53);
            case ManeuverIconEntryRing5 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou54);
            case ManeuverIconEntryRing6 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou55);
            case ManeuverIconEntryRing7 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou56);
            case ManeuverIconEntryRing8 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou57);
            case ManeuverIconEntryRing9 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou58);
            case ManeuverIconEntryRing10 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou59);
            case ManeuverIconLeaveRing1 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou60);
            case ManeuverIconLeaveRing2 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou61);
            case ManeuverIconLeaveRing3 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou62);
            case ManeuverIconLeaveRing4 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou63);
            case ManeuverIconLeaveRing5 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou64);
            case ManeuverIconLeaveRing6 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou65);
            case ManeuverIconLeaveRing7 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou66);
            case ManeuverIconLeaveRing8 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou67);
            case ManeuverIconLeaveRing9 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou68);
            case ManeuverIconLeaveRing10 -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou69);
            case ManeuverIconMergeLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou71);
            case ManeuverIconMergeRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou70);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    // 默认分时图标
    public static AutoUIDrawable getDrawableTimeLaneType(SceneCommonStruct.TimeLaneBottomAction value) {
        return switch (value) {
            case BackLaneBusWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_21_day);
            case BackLaneSpecialWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_24_day);
            case BackLaneTidalWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_25_day);
            case BackLaneReversibleWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_23_day);
            case BackLaneBusNoWorkable -> new AutoUIDrawable(
                    R.drawable.img_disable_lane_21_day);
            case BackLaneSpecialNoWorkable -> new AutoUIDrawable(
                    R.drawable.img_disable_lane_24_day);
            case BackLaneTidalNoWorkable -> new AutoUIDrawable(
                    R.drawable.img_disable_lane_25_day);
            case BackLaneReversibleNoWorkable -> new AutoUIDrawable(
                    R.drawable.img_disable_lane_23_day);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    // 推荐车道
    public static AutoUIDrawable getDrawableRecommendLane(SceneCommonStruct.LaneAction value) {
        return switch (value) {
            case LaneActionAheadAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_0_day);
            case LaneActionLeftAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_1_day);
            case LaneActionAheadLeftAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_2_day);
            case LaneActionAheadLeftAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_2_day);
            case LaneActionRightAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_3_day);
            case LaneActionAheadRightAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_4_day);
            case LaneActionAheadRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_4_day);
            case LaneActionLUTurnAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_5_day);
            case LaneActionLeftRightAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_6_day);
            case LaneActionLeftRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_6_day);
            case LaneActionAheadLeftRightAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_7_day);
            case LaneActionAheadLeftRightAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_7_day);
            case LaneActionAheadLeftRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_7_day);
            case LaneActionAheadLeftRightNoAction -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_7_day);
            case LaneActionRUTurnAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_8_day);
            case LaneActionAheadLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_9_day);
            case LaneActionAheadLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_9_day);
            case LaneActionAheadRUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_10_day);
            case LaneActionAheadRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_10_day);
            case LaneActionLeftLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_11_day);
            case LaneActionLeftLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_11_day);
            case LaneActionRightRUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_12_day);
            case LaneActionRightRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_12_day);
            case LaneActionLeftInAheadAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_13_13_day);
            case LaneActionAheadLeftLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_16_day);
            case LaneActionAheadLeftLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_16_day);
            case LaneActionAheadLeftLUTurnAndLUTrun -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_16_day);
            case LaneActionRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_17_day);
            case LaneActionRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_17_day);
            case LaneActionLeftRightLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_18_day);
            case LaneActionLeftRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_18_day);
            case LaneActionLeftRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_18_day);
            case LaneActionAheadRightLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_19_day);
            case LaneActionAheadRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_19_day);
            case LaneActionAheadRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_19_day);
            case LaneActionLeftRUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_20_day);
            case LaneActionLeftRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_20_day);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    // 分时车道
    public static AutoUIDrawable getDrawableTimeLane(SceneCommonStruct.LaneAction value) {
        switch (value) {
            case LaneActionAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_0_day);
            case LaneActionAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_0_day);
            case LaneActionLeftAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_1_day);
            case LaneActionLeftAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_1_day);
            case LaneActionAheadLeftAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_2_day);
            case LaneActionAheadLeftAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_2_day);
            case LaneActionAheadLeftNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_2_day);
            case LaneActionRightAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_3_day);
            case LaneActionRightAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_3_day);
            case LaneActionAheadRightAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_4_day);
            case LaneActionAheadRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_4_day);
            case LaneActionAheadRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_4_day);
            case LaneActionLUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_5_day);
            case LaneActionLUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_5_day);
            case LaneActionLeftRightAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_6_day);
            case LaneActionLeftRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_6_day);
            case LaneActionLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_6_day);
            case LaneActionAheadLeftRightAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_7_day);
            case LaneActionAheadLeftRightAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_7_day);
            case LaneActionAheadLeftRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_7_day);
            case LaneActionAheadLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_7_day);
            case LaneActionRUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_8_8_day);
            case LaneActionRUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_8_day);
            case LaneActionAheadLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_9_day);
            case LaneActionAheadLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_9_day);
            case LaneActionAheadLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_9_day);
            case LaneActionAheadRUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_10_day);
            case LaneActionAheadRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_8_10_day);
            case LaneActionAheadRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_10_day);
            case LaneActionLeftLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_11_day);
            case LaneActionLeftLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_11_day);
            case LaneActionLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_11_day);
            case LaneActionRightRUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_12_day);
            case LaneActionRightRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_8_12_day);
            case LaneActionRightRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_12_day);
            case LaneActionLeftInAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_13_13_day);
            case LaneActionLeftInAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_13_day);
            case LaneActionAheadLeftLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_16_day);
            case LaneActionAheadLeftLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_16_day);
            case LaneActionAheadLeftLUTurnAndLUTrun:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_16_day);
            case fLaneActionAheadLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_16_day);
            case LaneActionRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_17_day);
            case LaneActionRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_17_day);
            case LaneActionRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_17_day);
            case LaneActionLeftRightLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_18_day);
            case LaneActionLeftRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_18_day);
            case LaneActionLeftRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_18_day);
            case LaneActionLeftRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_18_day);
            case LaneActionAheadRightLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_0_19_day);
            case LaneActionAheadRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_3_19_day);
            case LaneActionAheadRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_5_19_day);
            case LaneActionAheadRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_19_day);
            case LaneActionLeftRUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_1_20_day);
            case LaneActionLeftRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_normal_lane_8_20_day);
            case LaneActionLeftRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_20_day);
            case LaneActionBusAndBus:
                return new AutoUIDrawable(
                        R.drawable.img_drivablelane_landfront_15_day);
            case LaneActionBusNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_undrivelane_landback_15_day);
            case LaneActionVariableAndVariable:
                return new AutoUIDrawable(
                        R.drawable.img_drivablelane_landfront_17_day);
            case LaneActionVariableNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_undrivelane_landback_17_day);
            case LaneActionDedicated:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_24_day);
            case LaneActionTidal:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_25_day);
            case LaneActionEmptyNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_undrivelane_landback_18_day);
            case LaneActionEmpty:
                return new AutoUIDrawable(
                        R.drawable.img_drivablelane_landfront_18_day);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }

    // 推荐分时
    public static AutoUIDrawable getDrawableRecommendTimeLane(SceneCommonStruct.LaneAction value) {
        return switch (value) {
            case LaneActionAheadAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_0_day);
            case LaneActionLeftAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_1_day);
            case LaneActionAheadLeftAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_2_day);
            case LaneActionAheadLeftAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_2_day);
            case LaneActionRightAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_3_day);
            case LaneActionAheadRightAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_4_day);
            case LaneActionAheadRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_4_day);
            case LaneActionLUTurnAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_5_day);
            case LaneActionLeftRightAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_6_day);
            case LaneActionLeftRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_6_day);
            case LaneActionAheadLeftRightAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_7_day);
            case LaneActionAheadLeftRightAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_7_day);
            case LaneActionAheadLeftRightAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_7_day);
            case LaneActionAheadLeftRightNoAction -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_7_day);
            case LaneActionRUTurnAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_8_day);
            case LaneActionAheadLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_9_day);
            case LaneActionAheadLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_9_day);
            case LaneActionAheadRUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_10_day);
            case LaneActionAheadRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_10_day);
            case LaneActionLeftLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_11_day);
            case LaneActionLeftLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_11_day);
            case LaneActionRightRUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_12_day);
            case LaneActionRightRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_12_day);
            case LaneActionLeftInAheadAndNUll -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_13_13_day);
            case LaneActionAheadLeftLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_16_day);
            case LaneActionAheadLeftLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_16_day);
            case LaneActionAheadLeftLUTurnAndLUTrun -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_16_day);
            case LaneActionRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_17_day);
            case LaneActionRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_17_day);
            case LaneActionLeftRightLUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_18_day);
            case LaneActionLeftRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_18_day);
            case LaneActionLeftRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_18_day);
            case LaneActionAheadRightLUTurnAndAhead -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_0_19_day);
            case LaneActionAheadRightLUTurnAndRight -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_3_19_day);
            case LaneActionAheadRightLUTurnAndLUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_5_19_day);
            case LaneActionLeftRUTurnAndLeft -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_1_20_day);
            case LaneActionLeftRUTurnAndRUTurn -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_8_20_day);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    // 推荐分时图标
    public static AutoUIDrawable getDrawableRecommendTimeLane(SceneCommonStruct.TimeLaneBottomAction value) {
        return switch (value) {
            case BackLaneBusWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_21_day);
            case BackLaneSpecialWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_24_day);
            case BackLaneTidalWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_25_day);
            case BackLaneReversibleWorkable -> new AutoUIDrawable(
                    R.drawable.img_optimal_lane_23_day);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    // 巡航车道图标
    public static AutoUIDrawable getDrawableCruiseLane(SceneCommonStruct.LaneAction value) {
        switch (value) {
            case LaneActionAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_0_day);
            case LaneActionAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_0_day);
            case LaneActionLeftAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_1_day);
            case LaneActionLeftAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_1_day);
            case LaneActionAheadLeftNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_2_day);
            case LaneActionRightAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_3_day);
            case LaneActionRightAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_3_day);
            case LaneActionAheadRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_4_day);
            case LaneActionLUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_5_day);
            case LaneActionLUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_5_day);
            case LaneActionLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_6_day);
            case LaneActionAheadLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_7_day);
            case LaneActionRUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_8_day);
            case LaneActionRUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_8_day);
            case LaneActionAheadLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_9_day);
            case LaneActionAheadRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_10_day);
            case LaneActionLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_11_day);
            case LaneActionRightRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_12_day);
            case LaneActionLeftInAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_13_day);
            case LaneActionLeftInAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_13_day);
            case fLaneActionAheadLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_16_day);
            case LaneActionRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_17_day);
            case LaneActionLeftRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_18_day);
            case LaneActionAheadRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_19_day);
            case LaneActionLeftRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_20_day);
            case LaneActionBusAndBus:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_21_day);
            case LaneActionBusNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_21_day);
            case LaneActionVariableAndVariable:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_23_day);
            case LaneActionVariableNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_23_day);
            case LaneActionDedicated:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_24_day);
            case LaneActionTidal:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_25_day);
            case LaneActionEmptyNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_22_day);
            case LaneActionEmpty:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_22_day);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }

    // 默认车道图标
    public static AutoUIDrawable getDrawableDefaultLane(SceneCommonStruct.LaneAction value) {
        switch (value) {
            case LaneActionAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_0_day);
            case LaneActionAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_0_day);
            case LaneActionLeftAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_1_day);
            case LaneActionLeftAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_1_day);
            case LaneActionAheadLeftAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_2_day);
            case LaneActionAheadLeftAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_2_day);
            case LaneActionAheadLeftNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_2_day);
            case LaneActionRightAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_3_day);
            case LaneActionRightAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_3_day);
            case LaneActionAheadRightAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_4_day);
            case LaneActionAheadRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_4_day);
            case LaneActionAheadRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_4_day);
            case LaneActionLUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_5_day);
            case LaneActionLUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_5_day);
            case LaneActionLeftRightAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_6_day);
            case LaneActionLeftRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_6_day);
            case LaneActionLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_6_day);
            case LaneActionAheadLeftRightAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_7_day);
            case LaneActionAheadLeftRightAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_7_day);
            case LaneActionAheadLeftRightAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_7_day);
            case LaneActionAheadLeftRightNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_7_day);
            case LaneActionRUTurnAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_8_8_day);
            case LaneActionRUTurnAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_8_day);
            case LaneActionAheadLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_9_day);
            case LaneActionAheadLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_9_day);
            case LaneActionAheadLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_9_day);
            case LaneActionAheadRUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_10_day);
            case LaneActionAheadRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_8_10_day);
            case LaneActionAheadRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_10_day);
            case LaneActionLeftLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_11_day);
            case LaneActionLeftLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_11_day);
            case LaneActionLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_11_day);
            case LaneActionRightRUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_12_day);
            case LaneActionRightRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_8_12_day);
            case LaneActionRightRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_12_day);
            case LaneActionLeftInAheadAndNUll:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_13_13_day);
            case LaneActionLeftInAheadAndNUllNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_13_day);
            case LaneActionAheadLeftLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_16_day);
            case LaneActionAheadLeftLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_16_day);
            case LaneActionAheadLeftLUTurnAndLUTrun:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_16_day);
            case fLaneActionAheadLeftLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_16_day);
            case LaneActionRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_17_day);
            case LaneActionRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_17_day);
            case LaneActionRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_17_day);
            case LaneActionLeftRightLUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_18_day);
            case LaneActionLeftRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_18_day);
            case LaneActionLeftRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_18_day);
            case LaneActionLeftRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_18_day);
            case LaneActionAheadRightLUTurnAndAhead:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_0_19_day);
            case LaneActionAheadRightLUTurnAndRight:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_3_19_day);
            case LaneActionAheadRightLUTurnAndLUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_5_19_day);
            case LaneActionAheadRightLUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_19_day);
            case LaneActionLeftRUTurnAndLeft:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_1_20_day);
            case LaneActionLeftRUTurnAndRUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_8_20_day);
            case LaneActionLeftRUTurnNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_20_day);
            case LaneActionBusAndBus:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_21_day);
            case LaneActionBusNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_21_day);
            case LaneActionVariableAndVariable:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_23_day);
            case LaneActionVariableNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_23_day);
            case LaneActionDedicated:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_24_day);
            case LaneActionTidal:
                return new AutoUIDrawable(
                        R.drawable.img_cruise_lane_25_day);
            case LaneActionEmptyNoAction:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_22_day);
            case LaneActionEmpty:
                return new AutoUIDrawable(
                        R.drawable.img_disable_lane_22_day);
            case LaneLabor:
                return new AutoUIDrawable(
                        R.drawable.img_lane_labor_day);
            case LaneEtc:
                return new AutoUIDrawable(
                        R.drawable.img_lane_etc_day);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }

    public static AutoUIDrawable getDrawableEnumName0(SceneCommonStruct.TmcViaPointType value) {
        switch (value) {
            case ViaPointType:
                return new AutoUIDrawable(
                        R.drawable.img_tmc_via);
            case ViaChargeType:
                return new AutoUIDrawable(
                        R.drawable.img_tmc_charge);
            case ViaStarBucksType:
                return new AutoUIDrawable(
                        R.drawable.img_trip_lineviastarbucks_day);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }
}
