package com.fy.navi.service.adapter.search.cloudByPatac.api;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.adapter.search.cloudByPatac.req.StationReq;

import io.reactivex.Observable;

public interface SearchApi {
    // 充电站列表查询
    Observable<BaseRep> queryStationNewResult(StationReq req);
    // 获取用户收藏充电站
    Observable<BaseRep> queryCollectStation(StationReq req);
    // 修改预约
    Observable<BaseRep> updateReservation(StationReq req);
    // 创建公桩预约
    Observable<BaseRep> createReservation(StationReq req);
    // 开启地锁
    Observable<BaseRep> unLockStation(StationReq req);
    // 更新用户收藏充电站
    Observable<BaseRep> updateCollectStation(StationReq req);
    // 查询公桩预约
    Observable<BaseRep> queryReservation(StationReq req);
    // 获取充电桩信息
    Observable<BaseRep> queryEquipmentInfo(StationReq req);
    // 获取充电站详情
    Observable<BaseRep> queryStationInfo(StationReq req);
}
