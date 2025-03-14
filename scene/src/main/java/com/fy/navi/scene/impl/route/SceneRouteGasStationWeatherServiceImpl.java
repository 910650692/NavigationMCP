package com.fy.navi.scene.impl.route;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherService;
import com.fy.navi.scene.ui.route.SceneRouteGasStationWeatherServiceView;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.logicpaket.route.RoutePackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteGasStationWeatherServiceImpl extends BaseSceneModel<SceneRouteGasStationWeatherServiceView> implements ISceneRouteGasStationWeatherService {

    private final RoutePackage mRoutePackage;
    public boolean isGasStationSelect = false;
    public boolean isWeatherSelect = false;
    public boolean isServiceSelect = false;

    public SceneRouteGasStationWeatherServiceImpl(SceneRouteGasStationWeatherServiceView mScreenView) {
        super(mScreenView);
        mRoutePackage = RoutePackage.getInstance();
    }
    @Override
    public void clickGasStation() {
        isGasStationSelect = !isGasStationSelect;
        isWeatherSelect = false;
        isServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(1, isWeatherSelect);
        mScreenView.clickTab(2, isServiceSelect);
        mScreenView.clickTab(0, isGasStationSelect);
    }

    @Override
    public void clickWeather() {
        isWeatherSelect = !isWeatherSelect;
        isGasStationSelect = false;
        isServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0, isGasStationSelect);
        mScreenView.clickTab(2, isServiceSelect);
        mScreenView.clickTab(1, isWeatherSelect);
    }

    @Override
    public void clickService() {
        isServiceSelect = !isServiceSelect;
        isGasStationSelect = false;
        isWeatherSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0, isGasStationSelect);
        mScreenView.clickTab(1, isWeatherSelect);
        mScreenView.clickTab(2, isServiceSelect);
    }
}
