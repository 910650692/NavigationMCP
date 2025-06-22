package com.sgm.navi.scene.impl.route.common;

import com.sgm.navi.scene.R;
import com.sgm.navi.scene.impl.navi.common.AutoUIDrawable;

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
                        R.drawable.img_tbt_exit_hud_sou2);
            case MainActionTurnRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou3);
            case MainActionSlightLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou4);
            case MainActionSlightRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou5);
            case MainActionTurnHardLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou6);
            case MainActionTurnHardRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou7);
            case MainActionUTurn:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou8);
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
                        R.drawable.img_tbt_exit_hud_sou9);
            case MainActionMergeLeft:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou71);
            case MainActionMergeRight:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou70);
            case MainActionEntryRing:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou11);
            case MainActionLeaveRing:
                return new AutoUIDrawable(
                        R.drawable.img_tbt_exit_hud_sou12);
            default:
                return new AutoUIDrawable(0, 0, 0, 0);
        }
    }
}
