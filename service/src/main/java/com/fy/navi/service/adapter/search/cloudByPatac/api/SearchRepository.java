package com.fy.navi.service.adapter.search.cloudByPatac.api;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.NetMethodManager;
import com.fy.navi.service.adapter.search.cloudByPatac.req.StationReq;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.TestInfo;

import io.reactivex.Observable;

public class SearchRepository implements SearchApi{
    private static volatile SearchRepository mInstance;
    public static SearchRepository getInstance() {
        if (null == mInstance) {
            synchronized (SearchRepository.class) {
                if (null == mInstance) {
                    mInstance = new SearchRepository();
                }
            }
        }
        return mInstance;
    }
    @Override
    public Observable<TestInfo> queryStationNewResult(StationReq req) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"dopost");
        Observable<TestInfo> observable = NetMethodManager.getInstance().doPost(SearchApiService.CLOUD_QUERY_STATION_NEW,req,TestInfo.class);
        return observable;
    }
}
