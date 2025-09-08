package com.sgm.navi.scene.impl.navi.common;

import com.sgm.navi.scene.R;

public final class SceneEnumRes {

    private SceneEnumRes() {

    }

    /**
     * @param value value
     * @return drawable
     */
    public static AutoUIDrawable getDrawableEnumName(
            final SceneCommonStruct.TbtExitIconAction value) {
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
            case ManeuverIconCharge -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou64_charge);
            case ManeuverIconMergeLeft -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou71);
            case ManeuverIconMergeRight -> new AutoUIDrawable(
                    R.drawable.img_tbt_exit_hud_sou70);
            default -> new AutoUIDrawable(0, 0, 0, 0);
        };
    }

    /**
     * @param value value
     * @return drawable
     */
    // 默认分时图标
    public static AutoUIDrawable getDrawableTimeLaneType(
            final SceneCommonStruct.TimeLaneBottomAction value) {
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

    /**
     * @param value value
     * @return drawable
     */
    // 推荐车道
    public static AutoUIDrawable getDrawableRecommendLane(
            final SceneCommonStruct.LaneAction value) {
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

    /**
     * @param value value
     * @return drawable
     */
    // 分时车道
    public static AutoUIDrawable getDrawableTimeLane(final SceneCommonStruct.LaneAction value) {
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

    /**
     * @param value value
     * @return AutoUIDrawable
     */
    // 推荐分时
    public static AutoUIDrawable getDrawableRecommendTimeLane(
            final SceneCommonStruct.LaneAction value) {
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

    /**
     * @param value value
     * @return drawable
     */
    // 推荐分时图标
    public static AutoUIDrawable getDrawableRecommendTimeLane(
            final SceneCommonStruct.TimeLaneBottomAction value) {
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

    /**
     * @param value value
     * @return drawable
     */
    // 默认车道图标
    public static AutoUIDrawable getDrawableDefaultLane(final SceneCommonStruct.LaneAction value) {
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
            case LaneActionLeftInAheadAndNUllNoAction: // 750无效
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
                        R.drawable.img_optimal_lane_24_day);
            case LaneActionTidal:
                return new AutoUIDrawable(
                        R.drawable.img_optimal_lane_25_day);
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

    /**
     * @param value value
     * @return drawable
     */
    public static AutoUIDrawable getDrawableEnumName0(
            final SceneCommonStruct.TmcViaPointType value) {
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

    public static AutoUIDrawable getExtenBackground(final int type) {
        switch (type) {
            case 1:// 左拓展
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_left);
            case -1:// 左拓展非高亮
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_left_disable);
            case 2:// 右拓展
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_right);
            case -2:// 右拓展非高亮
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_right_disable);
            case 26:// 左车道变窄
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_leftup);
            case -26:// 左车道变窄非高亮
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_leftup_disable);
            case 27:// 右车道变窄
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_rightup);
            case -27:// 右车道变窄非高亮
                return new AutoUIDrawable(R.drawable.ic_lane_expansion_rightup_disable);
            default:
                return new AutoUIDrawable(R.drawable.img_disable_lane_empty);
        }
    }
}
