package com.fy.navi.service.adapter.position.bls;

import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.IPositionApi;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.setting.SettingConstant;

import java.math.BigInteger;

public class PositionAdapterImplD implements IPositionApi {
    @Override
    public void registerCallback(IPositionAdapterCallback callback) {

    }

    @Override
    public void unregisterCallback(IPositionAdapterCallback callback) {

    }

    @Override
    public void unInitPositionService() {

    }

    @Override
    public boolean init() {
        return true;
    }

    @Override
    public LocInfoBean getLastCarLocation() {
        LocInfoBean locInfoBean =  new LocInfoBean();
        locInfoBean.setLongitude(SettingConstant.DEFAULT_LON_SH);
        locInfoBean.setLatitude(SettingConstant.DEFAULT_LAT_SH);
        locInfoBean.setAltitude(SettingConstant.DEFAULT_ALT_SH);
        return locInfoBean;
    }

    @Override
    public void startPosition() {

    }

    @Override
    public void stopPosition() {

    }

    @Override
    public void saveLocStorage() {

    }

    @Override
    public void switchParallelRoad(int switchRoadType, BigInteger roadId) {

    }

    @Override
    public GeoPoint wgs84ToGcj02(GeoPoint geoPoint) {
        return new GeoPoint();
    }

    @Override
    public void onGearChanged(int gear) {

    }

    @Override
    public void setDrEnable(boolean enable) {

    }

    @Override
    public void setCustomPOI(double lon, double lat) {

    }

    @Override
    public void locationLogSwitch(boolean isOpen) {

    }

    @Override
    public void onSpeedChanged(float speed) {

    }
}
