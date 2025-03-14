package com.fy.navi.scene.impl.route.common;

public class SceneRouteCommonStruct {

    public enum RouteDetailsMainAction {
        MainActionTurnLeft(1),
        MainActionTurnRight(2),
        MainActionSlightLeft(3),
        MainActionSlightRight(4),
        MainActionTurnHardLeft(5),
        MainActionTurnHardRight(6),
        MainActionUTurn(7),
        MainActionContinue(8),
        MainActionMergeLeft(9),
        MainActionMergeRight(10),
        MainActionEntryRing(11),
        MainActionLeaveRing(12),
        MainActionSlow(13),
        MainActionPlugContinue(14),
        MainActionEnterBuilding(65),
        MainActionLeaveBuilding(66),
        MainActionByElevator(67),
        MainActionByStair(68),
        MainActionByEscalator(69),
        MainActionCount(70);



        private final Object value;

        RouteDetailsMainAction(Object value) {
            this.value = value;
        }

        public static RouteDetailsMainAction get(Object value) {
            for (RouteDetailsMainAction obj : values()) {
                if (obj.value.equals(value)) {
                    return obj;
                }
            }
            return values()[0];
        }
    }
}
