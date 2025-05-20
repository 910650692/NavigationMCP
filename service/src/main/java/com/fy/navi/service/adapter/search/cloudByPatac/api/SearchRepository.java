package com.fy.navi.service.adapter.search.cloudByPatac.api;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.NetMethodManager;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.adapter.search.cloudByPatac.req.StationReq;

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
    public Observable<String> queryStationNewResult(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.CLOUD_QUERY_STATION_NEW,req);
        return observable;
    }

    @Override
    public Observable<String> queryCollectStation(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.CLOUD_QUERY_COLLECT_STATION,req);
        return observable;
    }

    @Override
    public Observable<BaseRep> updateReservation(StationReq req) {
        Observable<BaseRep> observable = NetMethodManager.getInstance().doPost(SearchApiService.UPDATE_RESERVATION,req, BaseRep.class);
        return observable;
    }

    @Override
    public Observable<String> createReservation(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.CREATE_RESERVATION_STATION,req);
        return observable;
    }

    @Override
    public Observable<String> unLockStation(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.CLOUD_QUERY_UNLOCK,req);
        return observable;
    }

    @Override
    public Observable<String> updateCollectStation(StationReq req,String json) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.UPDATE_COLLECT_STATION,req,json);
        return observable;
    }

    @Override
    public Observable<String> queryReservation(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.QUERY_RESERVATION_STATION,req);
        return observable;
    }

    @Override
    public Observable<String> queryEquipmentInfo(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.QUERY_EQUIPMENT_INFO,req);
        return observable;
    }

    @Override
    public Observable<String> queryStationInfo(StationReq req) {
        Observable<String> observable = NetMethodManager.getInstance().doPostReturnAllJson(SearchApiService.QUERY_STATION_INFO,req);
        return observable;
    }
}
