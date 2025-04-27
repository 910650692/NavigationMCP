package com.fy.navi.service.adapter.search.cloudByPatac.api;
import com.fy.navi.service.adapter.search.cloudByPatac.req.StationReq;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.TestInfo;

import io.reactivex.Observable;

public interface SearchApi {
    /**
     * Post 请求  充电站列表查询
     *
     * @return Observable<PoiInfoEntity></PoiInfoEntity>
     */
    Observable<TestInfo> queryStationNewResult(StationReq req);
}
