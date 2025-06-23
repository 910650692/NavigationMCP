package com.sgm.navi.service.adapter.position;


import com.sgm.navi.service.define.navi.L2NaviBean;
import com.sgm.navi.service.define.position.DrBean;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocMMInfo;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.user.usertrack.GpsTrackPointBean;

public interface IPositionAdapterCallback {

   default void onLocationInfo(LocInfoBean locationInfo){}

    /***DR标定信息***/
    default  void onLocAnalysisResult(@PositionConstant.DRDebugEvent int infoType, String info){}

    /***平行路切换完成***/
    default void onSwitchParallelRoadFinished(){}

    /***更新主辅路信息***/
    default void onParallelRoadUpdate(LocParallelInfoEntity entity){}

    /***Gps 状态改变回调***/
    default void onGpsSatellitesChanged(boolean isLocSuccess){}

    /*** 开启L2++时调用 **/
    default void onGraspRouteResult(L2NaviBean.VehiclePositionBean vehiclePosition){}

    /*** 卫星数 **/
    default void onSatelliteNum(int num){}

    /*** 回调最后位置打点信息 **/
    default void onGpsTrackPoint(GpsTrackPointBean gpsTrackPoi0903ntBean){}
}
