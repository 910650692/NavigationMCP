package com.fy.navi.service.adapter.position;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.adapter.signal.SignalAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.position.LocMode;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.PositionConfig;

import java.math.BigInteger;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class PositionAdapter {

    private IPositionApi mPositionApi;


    private static class PositionAdapterHolder {
        private static final PositionAdapter mInstance = new PositionAdapter();
    }

    public static PositionAdapter getInstance() {
        return PositionAdapterHolder.mInstance;
    }

    private PositionAdapter() {
        mPositionApi = (IPositionApi) AdapterConfig.getObject(Objects.requireNonNull(this.getClass().getPackage()).getName(), "PositionAdapterImpl");
    }

    public void init() {
        mPositionApi.init();
    }

    public void registerCallback(IPositionAdapterCallback callback) {
        mPositionApi.registerCallback(callback);
    }

    public void unregisterCallback(IPositionAdapterCallback callback){
        mPositionApi.unregisterCallback(callback);
    }

    public void unInitPositionService() {
        mPositionApi.unInitPositionService();
    }

    public LocInfoBean getLastCarLocation() {
        return mPositionApi.getLastCarLocation();
    }

    public void startPosition() {
        mPositionApi.startPosition();
    }

    public void saveLocStorage() {
        mPositionApi.saveLocStorage();
    }

    public void stopPosition() {
        mPositionApi.stopPosition();
    }

    /***切换主辅路、高架***/
    public void switchParallelRoad(int switchRoadType, BigInteger roadId) {
        mPositionApi.switchParallelRoad(switchRoadType, roadId);
    }

    public GeoPoint wgs84ToGcj02(GeoPoint geoPoint) {
        return mPositionApi.wgs84ToGcj02(geoPoint);
    }

    /**
     * @param gear 挡位
     */
    public void onGearChanged(int gear) {
        mPositionApi.onGearChanged(gear);
    }

    public void setDrEnable(boolean enable) {
        mPositionApi.setDrEnable(enable);
    }

    public void setCustomPOI(double lon, double lat) {
        mPositionApi.setCustomPOI(lon, lat);
    }

    public void locationLogSwitch(boolean isOpen) {
        mPositionApi.locationLogSwitch(isOpen);
    }
}
