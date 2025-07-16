package com.sgm.navi.adas;

import com.gm.cn.adassdk.proto.RouteInfo;

public enum ScTbtIcon {
    ManeuverIconNull(0, RouteInfo.ManeuverType.UNKNOWN),
    ManeuverIconTurnLeft(2, RouteInfo.ManeuverType.TURN_NORMAL_LEFT),
    ManeuverIconTurnRight(3, RouteInfo.ManeuverType.TURN_NORMAL_RIGHT),
    ManeuverIconSlightLeft(4, RouteInfo.ManeuverType.TURN_SLIGHT_LEFT),
    ManeuverIconSlightRight(5, RouteInfo.ManeuverType.TURN_SLIGHT_RIGHT),
    ManeuverIconTurnHardLeft(6, RouteInfo.ManeuverType.TURN_SHARP_LEFT),
    ManeuverIconTurnHardRight(7, RouteInfo.ManeuverType.TURN_SHARP_RIGHT),
    ManeuverIconUTurn(8, RouteInfo.ManeuverType.U_TURN_LEFT),
    ManeuverIconContinue(9, RouteInfo.ManeuverType.STRAIGHT),
    ManeuverIconWay(10, RouteInfo.ManeuverType.UNKNOWN),
    ManeuverIconEntryRing(11, RouteInfo.ManeuverType.ROUNDABOUT_ENTER),
    ManeuverIconLeaveRing(12, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconSAPA(13, RouteInfo.ManeuverType.UNKNOWN),
    ManeuverIconTollGate(14, RouteInfo.ManeuverType.UNKNOWN),
    ManeuverIconDestination(15, RouteInfo.ManeuverType.AT_DESTINATION),
    ManeuverIconTunnel(16, RouteInfo.ManeuverType.UNKNOWN),
    ManeuverIconEntryLeftRing(17, RouteInfo.ManeuverType.ROUNDABOUT_ENTER),
    ManeuverIconLeaveLeftRing(18, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconUTurnRight(19, RouteInfo.ManeuverType.U_TURN_RIGHT),
    ManeuverIconSpecialContinue(20, RouteInfo.ManeuverType.STRAIGHT),
    fManeuverIconEntryRingLeft(21, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRingRight(22, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRingContinue(23, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRingUTurn(24, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryLeftRingLeft(25, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    fManeuverIconEntryLeftRingRight(26, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryLeftRingContinue(27, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryLeftRingUTurn(28, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing1(68, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing2(69, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing3(70, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing4(71, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing5(72, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing6(73, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing7(74, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing8(75, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing9(76, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconEntryRing10(77, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing1(79, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing2(80, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing3(81, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing4(82, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing5(83, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing6(84, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing7(85, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing8(86, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing9(87, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconLeaveRing10(88, RouteInfo.ManeuverType.ROUNDABOUT_EXIT),
    ManeuverIconMergeLeft(65, RouteInfo.ManeuverType.ON_RAMP_LEFT),
    ManeuverIconMergeRight(66, RouteInfo.ManeuverType.ON_RAMP_RIGHT);

    private final int amapValue;
    private final RouteInfo.ManeuverType scValue;

    ScTbtIcon(int amapValue, RouteInfo.ManeuverType scValue) {
        this.amapValue = amapValue;
        this.scValue = scValue;
    }

    public static RouteInfo.ManeuverType get(int amapValue) {
        for (ScTbtIcon scTbtIcon : values()) {
            if (scTbtIcon.amapValue == amapValue) {
                return scTbtIcon.scValue;
            }
        }
        return ManeuverIconNull.scValue;
    }
}
