package com.sgm.navi.service.adapter.position;

import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.position.LocInfoBean;

import java.math.BigInteger;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface IPositionApi {

    void registerCallback(IPositionAdapterCallback callback);

    void unregisterCallback(IPositionAdapterCallback callback);

    void unInitPositionService();

    /*定位服务初始化*/
    boolean init();

    /*获取当前位置*/
    LocInfoBean getLastCarLocation();

    void startPosition();

    void stopPosition();

    void saveLocStorage();

    void switchParallelRoad(int switchRoadType, BigInteger roadId);

    GeoPoint wgs84ToGcj02(GeoPoint geoPoint);

    /**
     * @param gear 挡位
     */
    void onGearChanged(int gear);

    void setDrEnable(boolean enable);

    void locationLogSwitch(boolean isOpen);

    void onSpeedChanged(float speed);
}
