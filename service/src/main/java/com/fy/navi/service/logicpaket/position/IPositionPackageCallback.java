package com.fy.navi.service.logicpaket.position;


/*
 * @Summary 对HMI的callback
 * @Author wangbo
 * @date 2024/11/29
 */

import com.fy.navi.service.adapter.position.PositionConstant;
import com.fy.navi.service.define.position.DrBean;
import com.fy.navi.service.define.position.LocMMInfo;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.position.LocStatus;
import com.fy.navi.service.define.position.LocInfoBean;

public interface IPositionPackageCallback {

    default void onLocationInfo(LocInfoBean locationInfo) {

    }

    default void onLocationStatus(LocStatus locStatus) {

    }

    default void onDrInfo(DrBean drInfo) {

    }

    /***DR标定信息***/
    default void onLocAnalysisResult(@PositionConstant.DRDebugEvent int infoType, String info) {
    }

    /***Gps 状态改变回调***/
    default void onGpsSatellitesChanged(boolean isLocSuccess) {
    }

    /***平行路切换完成***/
    default void onSwitchParallelRoadFinished() {
    }

    /***更新主辅路信息***/
    default void onParallelRoadUpdate(LocParallelInfoEntity entity) {
    }

    /***更新MM信息***/
    default void onMapMatchFeedbackUpdate(LocMMInfo locMMInfo) {
    }

    /*** 卫星数 **/
    default void onSatelliteNum(int num){}
}
