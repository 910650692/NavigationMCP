package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeService;
import com.fy.navi.scene.ui.route.SceneRouteGasStationChargeServiceView;

public class SceneRouteGasStationChargeServiceImpl extends BaseSceneModel<SceneRouteGasStationChargeServiceView>
        implements ISceneRouteGasStationChargeService {

    public boolean isGasStationSelect() {
        return isGasStationSelect;
    }

    public boolean isWeatherSelect() {
        return isWeatherSelect;
    }

    public boolean isServiceSelect() {
        return isServiceSelect;
    }

    public void setGasStationSelect(boolean gasStationSelect) {
        isGasStationSelect = gasStationSelect;
    }

    public void setWeatherSelect(boolean weatherSelect) {
        isWeatherSelect = weatherSelect;
    }

    public void setServiceSelect(boolean serviceSelect) {
        isServiceSelect = serviceSelect;
    }

    private boolean isGasStationSelect = false;
    private boolean isWeatherSelect = false;
    private boolean isServiceSelect = false;

    public SceneRouteGasStationChargeServiceImpl(SceneRouteGasStationChargeServiceView mScreenView) {
        super(mScreenView);
    }
    @Override
    public void clickAlone() {
        isGasStationSelect = true;
        isWeatherSelect = false;
        isServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0);
    }

    @Override
    public void clickEnd() {
        isWeatherSelect = true;
        isGasStationSelect = false;
        isServiceSelect = false;
        mScreenView.updateUi();
        mScreenView.clickTab(1);
    }

    @Override
    public void clickAround() {
        isServiceSelect = true;
        isGasStationSelect = false;
        isWeatherSelect = false;
        mScreenView.updateUi();
        mScreenView.clickTab(2);
    }
}
