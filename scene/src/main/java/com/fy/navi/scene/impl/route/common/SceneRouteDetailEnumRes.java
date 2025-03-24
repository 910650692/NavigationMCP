package com.fy.navi.scene.impl.route.common;

import com.fy.navi.scene.R;
import com.fy.navi.scene.impl.navi.common.AutoUIDrawable;

public interface SceneRouteDetailEnumRes {

    /**
     * 获取转向图片
     * @param value 图片值
     * @return icon 图片
     * */
    public static AutoUIDrawable getDrawableEnumName(final SceneRouteCommonStruct.RouteDetailsMainAction value) {

        switch (value) {
            case MainActionTurnLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou2_day);
            case MainActionTurnRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou3_day);
            case MainActionSlightLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou4_day);
            case MainActionSlightRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou5_day);
            case MainActionTurnHardLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou6_day);
            case MainActionTurnHardRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou7_day);
            case MainActionUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou8_day);
            case MainActionContinue:
            case MainActionSlow:
            case MainActionPlugContinue:
            case MainActionEnterBuilding:
            case MainActionLeaveBuilding:
            case MainActionByElevator:
            case MainActionByStair:
            case MainActionByEscalator:
            case MainActionCount:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou9_day);
            case MainActionMergeLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou71_day);
            case MainActionMergeRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou70_day);
            case MainActionEntryRing:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou11_day);
            case MainActionLeaveRing:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_hud_sou12_day);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }
}
