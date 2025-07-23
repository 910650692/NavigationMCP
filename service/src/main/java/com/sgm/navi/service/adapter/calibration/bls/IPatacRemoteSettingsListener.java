package com.sgm.navi.service.adapter.calibration.bls;

import patac.manager.setting.PatacRemoteSettingsManager;

public class IPatacRemoteSettingsListener implements PatacRemoteSettingsManager.PatacRemoteSettingsListener {


    @Override
    public void clientSubscriberConnected() {

    }

    @Override
    public void onAllHudSettingInfoRequest(PatacRemoteSettingsManager.HudSettingInfo hudSettingInfo) {

    }

    @Override
    public void onHudSettingChangedRequest(PatacRemoteSettingsManager.EHudSettingType eHudSettingType, int i) {

    }

    @Override
    public void onChangeHudAdjustmentResultRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType) {

    }

    @Override
    public void onChangeHudAdjustmentInfoRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType, float v) {

    }

    @Override
    public void onHudCustomInfoRequest(PatacRemoteSettingsManager.EHudAdjustmentType eHudAdjustmentType, boolean b) {

    }

    @Override
    public void onSettingClusterViewChangedRequest(PatacRemoteSettingsManager.ECurrentView eCurrentView) {

    }

    @Override
    public void onSettingClusterZone2CurrentPageRequest(PatacRemoteSettingsManager.ECurrentPage eCurrentPage) {

    }

    @Override
    public void onAllClusterSettingInfoRequest(PatacRemoteSettingsManager.ClusterSettingInfo clusterSettingInfo) {

    }

    @Override
    public void onEasterEggRequest() {

    }

    @Override
    public void onSettingChangeNaviVisibilityInZone3Request(boolean b) {

    }

    @Override
    public void onSettingUpdateSpeedDisplayOptionRequest(boolean b) {

    }

    @Override
    public void onSettingUpdateSpeedLimitSourceRequest(PatacRemoteSettingsManager.ESpeedLimit eSpeedLimit) {

    }

    @Override
    public void onSettingUpdateSpeedLimitWarningVisibilityRequest(boolean b) {

    }

    @Override
    public void onSettingUpdateSpeedLimitVisibilityRequest(boolean b) {

    }

    @Override
    public void onZone2PageStatusRequest(boolean b, PatacRemoteSettingsManager.ECurrentPage eCurrentPage) {

    }

    @Override
    public void onLampIconClickedRequest() {

    }

    @Override
    public void onSmartControllerOnRequest(PatacRemoteSettingsManager.EFunctionType eFunctionType) {

    }

    @Override
    public void onSettingChangeClusterSmartControlRequest(int i) {

    }

    @Override
    public void onSettingChangeVehicleModelColorRequest(int i) {

    }

    @Override
    public void onButtonBrightnessRequest(int i) {

    }

    @Override
    public void onDispalyBrightnessRequest(int i) {

    }
}
