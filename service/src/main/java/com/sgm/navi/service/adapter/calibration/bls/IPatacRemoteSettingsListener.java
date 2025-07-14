package com.sgm.navi.service.adapter.calibration.bls;

import patac.manager.setting.PatacRemoteSettingsManager;

public interface IPatacRemoteSettingsListener extends PatacRemoteSettingsManager.PatacRemoteSettingsListener {

    default void clientSubscriberConnected() {

    }

    default void onAllHudSettingInfoRequest(PatacRemoteSettingsManager.HudSettingInfo hudSettingInfo) {

    }

    default void onHudSettingChangedRequest(PatacRemoteSettingsManager.EHudSettingType eHudSettingType, int i) {

    }

    default void onChangeHudAdjustmentResultRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType) {

    }

    default void onChangeHudAdjustmentInfoRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType, float v) {

    }

    default void onHudCustomInfoRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType, boolean b) {

    }

    default void onSettingClusterViewChangedRequest(PatacRemoteSettingsManager.ECurrentView eCurrentView) {

    }

    default void onSettingClusterZone2CurrentPageRequest(PatacRemoteSettingsManager.ECurrentPage eCurrentPage) {

    }

    default void onAllClusterSettingInfoRequest(PatacRemoteSettingsManager.ClusterSettingInfo clusterSettingInfo) {

    }

    default void onEasterEggRequest() {

    }

    default void onSettingChangeNaviVisibilityInZone3Request(boolean b) {

    }

    default void onSettingUpdateSpeedDisplayOptionRequest(boolean b) {

    }

    default void onSettingUpdateSpeedLimitSourceRequest(PatacRemoteSettingsManager.ESpeedLimit eSpeedLimit) {

    }

    default void onSettingUpdateSpeedLimitWarningVisibilityRequest(boolean b) {

    }

    default void onSettingUpdateSpeedLimitVisibilityRequest(boolean b) {

    }

    default void onZone2PageStatusRequest(boolean b, PatacRemoteSettingsManager.ECurrentPage eCurrentPage) {

    }

    default void onLampIconClickedRequest() {

    }

    default void onSmartControllerOnRequest(PatacRemoteSettingsManager.EFunctionType eFunctionType) {

    }

    default void onSettingChangeClusterSmartControlRequest(int i) {

    }

    default void onSettingChangeVehicleModelColorRequest(int i) {

    }

    default void onButtonBrightnessRequest(int i) {

    }

    default void onDispalyBrightnessRequest(int i) {

    }
}
