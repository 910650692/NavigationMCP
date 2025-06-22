package com.sgm.navi.service.define.navi;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public final class PlayModule {
    public static final int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
    public static final int PlayModuleInvalid = -1;
    public static final int PlayModuleRoute = 0;
    public static final int PlayModuleNavi = 1;
    public static final int PlayModuleViaPoint = 2;
    public static final int PlayModuleDiyPlay = 3;
    public static final int PlayModuleCamera = 4;
    public static final int PlayModuleLane = 5;
    public static final int PlayModuleLocal = 6;
    public static final int PlayModuleCityChange = 7;
    public static final int PlayModuleSAPA = 8;
    public static final int PlayModuleNoTurn = 9;
    public static final int PlayModuleBridge = 10;
    public static final int PlayModuleFreeWay = 11;
    public static final int PlayModuleLinkTurn = 12;
    public static final int PlayModuleTime = 13;
    public static final int PlayModuleTurnLane = 14;
    public static final int PlayModuleLightStraight = 15;
    public static final int PlayModuleMixForks = 16;
    public static final int PlayModuleIntervalCamera = 17;
    public static final int PlayModuleSolidLane = 18;
    public static final int PlayModuleFacilityAvoid = 19;
    public static final int PlayModuleForbidden = 20;
    public static final int PlayModuleCheckPoint = 21;
    public static final int PlayModuleSign = 22;
    public static final int PlayModuleGlobalKeep = 23;
    public static final int PlayModuleCurve = 24;
    public static final int PlayModuleContinueLinkTurn = 25;
    public static final int PlayModuleTrafficStatus = 26;
    public static final int PlayModuleSwitchParallelRoad = 27;
    public static final int PlayModuleSwitchPath = 28;
    public static final int PlayModulPassVia = 29;
    public static final int PlayModuleWeatherPlay = 30;
    public static final int PlayModuleAbormalAction = 31;
    public static final int PlayModuleAliPayTollGate = 32;
    public static final int PlayModuleTrafficSignal = 33;
    public static final int PlayModuleGreenLine = 34;
    public static final int PlayModuleEscortIncident = 35;
    public static final int PlayModuleOppositeWarn = 36;
    public static final int PlayModuleParkAlipay = 37;
    public static final int PlayModuleEscort = 38;
    public static final int PlayModuleThreeUrgent = 39;
    public static final int PlayModuleCustomization = 40;
    public static final int PlayModulehwReroute = 41;
    public static final int PlayModeleDynamicInfo = 42;
    public static final int PlayModuleVoiceAssistant = 101;
    public static final int PlayModuleOtherModule = 128;
    public static final int PlayModuleLaneNavi = 10000;
    public static final int PlayModuleCount = 10001;

    public PlayModule() {
    }

    @Retention(RetentionPolicy.CLASS)
    public @interface PlayModule1 {
    }
}
