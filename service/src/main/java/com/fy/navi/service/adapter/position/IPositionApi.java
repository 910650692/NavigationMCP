package com.fy.navi.service.adapter.position;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.position.LocMode;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.PositionConfig;

import java.math.BigInteger;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface IPositionApi {

    void registerCallback(IPositionAdapterCallback callback);

    void unInitPositionService();

    /*定位服务初始化*/
    void init();

    /*获取当前位置*/
    LocInfoBean getLastCarLocation();

    void startPosition();

    void stopPosition();

    void saveLocStorage();

    void switchParallelRoad(int switchRoadType, BigInteger roadId);

    GeoPoint wgs84ToGcj02(GeoPoint geoPoint);

    /*后端融合是否生效 true：生效 false：禁用*/
    void setDrBackFusionEnable(boolean enable);

    /*开启dr录制 true：生效 false：禁用（此接口为预留）*/
    void setRecordEnable(boolean enable);

    /**
     * @param gear 挡位
     */
    void onGearChanged(int gear);

    void setDrEnable(boolean enable);

    /*自定义起点*/
    void setCustomPOI(double lon, double lat);

    void locationLogSwitch(boolean isOpen);
}
