package com.sgm.navi.service.adapter.cruise;

import com.sgm.navi.service.adapter.navi.BaseNaviObserver;
import com.sgm.navi.service.define.cruise.CruiseFacilityEntity;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface CruiseObserver extends BaseNaviObserver {
    /*车道线信息*/
    void onCruiseLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo);

    /*传出巡航探测到的电子眼*/
    void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity);

    /*巡航过程中传出巡航状态下的信息*/
    void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity);

    /*巡航播报前方路况 true：开启 false：关闭*/
    void setConfigKeyRoadWarn(boolean roadWarn);

    /*巡航播报电子眼播报 true：开启 false：关闭*/
    void setConfigKeySafeBroadcast(boolean safeBroadcast);

    /*巡航播报安全提醒 true：开启 false：关闭*/
    void setConfigKeyDriveWarn(boolean driveWarn);

    void onUpdateCruiseFacility(CruiseFacilityEntity cruiseFacilityEntity);

//    void onCruiseIntervalvelocity(CruiseIntervalvelocity cruiseIntervalvelocity);
}
