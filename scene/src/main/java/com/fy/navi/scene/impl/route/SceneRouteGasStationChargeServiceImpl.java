package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeService;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherService;
import com.fy.navi.scene.ui.route.SceneRouteGasStationChargeServiceView;
import com.fy.navi.scene.ui.route.SceneRouteGasStationWeatherServiceView;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteGasStationChargeServiceImpl extends BaseSceneModel<SceneRouteGasStationChargeServiceView> implements ISceneRouteGasStationChargeService {

    private final RoutePackage mRoutePackage;
    public boolean isGasStationSelect = false;
    public boolean isWeatherSelect = false;
    public boolean isServiceSelect = false;

    public SceneRouteGasStationChargeServiceImpl(SceneRouteGasStationChargeServiceView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
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
