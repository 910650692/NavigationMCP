package com.fy.navi.scene.impl.route;

import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherService;
import com.fy.navi.scene.ui.route.SceneRouteGasStationWeatherServiceView;

public class SceneRouteGasStationWeatherServiceImpl extends BaseSceneModel<SceneRouteGasStationWeatherServiceView>
        implements ISceneRouteGasStationWeatherService {

    public boolean isGasStationSelect() {
        return mIsChargingStationSelect;
    }

    public void setGasStationSelect(final boolean isGasStationSelect) {
        this.mIsGasStationSelect = isGasStationSelect;
    }

    public boolean isChargingStationSelect() {
        return mIsChargingStationSelect;
    }

    public void setChargingStationSelect(final boolean isChargingStationSelect) {
        this.mIsChargingStationSelect = isChargingStationSelect;
    }

    public boolean isWeatherSelect() {
        return mIsWeatherSelect;
    }

    public void setWeatherSelect(final boolean isWeatherSelect) {
        this.mIsWeatherSelect = isWeatherSelect;
    }

    public boolean isServiceSelect() {
        return mIsServiceSelect;
    }

    public void setServiceSelect(final boolean isServiceSelect) {
        this.mIsServiceSelect = isServiceSelect;
    }

    private boolean mIsChargingStationSelect = false;
    private boolean mIsGasStationSelect = false;
    private boolean mIsWeatherSelect = false;
    private boolean mIsServiceSelect = false;

    public SceneRouteGasStationWeatherServiceImpl(final SceneRouteGasStationWeatherServiceView serviceView) {
        super(serviceView);
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_ROUTE_GASSTATION)
    public void clickGasStation() {
        mIsGasStationSelect = !mIsGasStationSelect;
        mIsWeatherSelect = false;
        mIsChargingStationSelect = false;
        mIsServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0, mIsChargingStationSelect);
        mScreenView.clickTab(2, mIsServiceSelect);
        mScreenView.clickTab(1, mIsWeatherSelect);
        mScreenView.clickTab(3, mIsGasStationSelect);
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_ROUTE_CHARGINGSTATION)
    public void clickChargingStation() {
        mIsChargingStationSelect = !mIsChargingStationSelect;
        mIsGasStationSelect = false;
        mIsWeatherSelect = false;
        mIsServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(1, mIsWeatherSelect);
        mScreenView.clickTab(2, mIsServiceSelect);
        mScreenView.clickTab(0, mIsChargingStationSelect);
        mScreenView.clickTab(3, mIsGasStationSelect);
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_ROUTE_WEATHER)
    public void clickWeather() {
        mIsWeatherSelect = !mIsWeatherSelect;
        mIsGasStationSelect = false;
        mIsChargingStationSelect = false;
        mIsServiceSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0, mIsChargingStationSelect);
        mScreenView.clickTab(2, mIsServiceSelect);
        mScreenView.clickTab(1, mIsWeatherSelect);
        mScreenView.clickTab(3, mIsGasStationSelect);
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_ROUTE_SERVICE)
    public void clickService() {
        mIsServiceSelect = !mIsServiceSelect;
        mIsGasStationSelect = false;
        mIsChargingStationSelect = false;
        mIsWeatherSelect = false;
        mScreenView.updateUi();

        mScreenView.clickTab(0, mIsChargingStationSelect);
        mScreenView.clickTab(1, mIsWeatherSelect);
        mScreenView.clickTab(2, mIsServiceSelect);
        mScreenView.clickTab(3, mIsGasStationSelect);
    }
}
