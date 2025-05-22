package com.fy.navi.service.adapter.search.cloudByPatac.api;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.adapter.search.cloudByPatac.req.StationReq;

import io.reactivex.Observable;

public interface SearchApi {
    // 充电站列表查询
    Observable<String> queryStationNewResult(StationReq req);
    // 获取用户收藏充电站
    Observable<String> queryCollectStation(StationReq req);
    // 修改预约
    Observable<String> updateReservation(StationReq req);
    // 创建公桩预约
    Observable<String> createReservation(StationReq req);
    // 开启地锁
    Observable<String> unLockStation(StationReq req);
    // 更新用户收藏充电站
    Observable<String> updateCollectStation(StationReq req,String json);
    // 查询公桩预约
    Observable<String> queryReservation(StationReq req);
    // 获取充电桩信息
    Observable<String> queryEquipmentInfo(StationReq req);
    // 获取充电站详情
    Observable<String> queryStationInfo(StationReq req);
}
