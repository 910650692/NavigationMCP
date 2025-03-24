package com.fy.navi.service.adapter.hotupdate.bls;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.data.HotUpdateService;
import com.autonavi.gbl.data.model.MapNum;
import com.autonavi.gbl.data.observer.IMapNumObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.adapter.hotupdate.HotUpdateAdapterCallback;
import com.fy.navi.service.adapter.hotupdate.HotUpdateApi;
import com.fy.navi.service.define.hotupdate.MapNumInfo;

import java.util.Hashtable;

public class HotUpdateAdapterImpl implements HotUpdateApi, IMapNumObserver {

    private HotUpdateService mHotUpdateService;
    private final Hashtable<String, HotUpdateAdapterCallback> mHotUpdateHashtable;


    public HotUpdateAdapterImpl() {
        mHotUpdateHashtable = new Hashtable<>();
    }


    @Override
    public int requestMapNum(final MapNumInfo mapNumInfo) {
        final MapNum mapNum = new MapNum();
        mapNum.strKey = mapNumInfo.getStrKey();
        mapNum.strVersion = mapNumInfo.getStrVersion();
        mapNum.strContent = mapNumInfo.getStrContent();
        Logger.d("requestMapNum: " + GsonUtils.toJson(mapNum));
        return mHotUpdateService.requestMapNum(mapNum, this);
    }

    @Override
    public void initService() {
        mHotUpdateService = (HotUpdateService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.HotUpdateSingleServiceID);
    }

    @Override
    public void registerCallback(final String key, final HotUpdateAdapterCallback resultCallback) {
        mHotUpdateHashtable.put(key, resultCallback);
    }

    @Override
    public void onRequestMapNum(final int i, final MapNum mapNum) {
        Logger.d("onRequestMapNum: " + GsonUtils.toJson(mapNum));
        for (HotUpdateAdapterCallback hotUpdateAdapterCallback : mHotUpdateHashtable.values()) {
            hotUpdateAdapterCallback.onRequestMapNum(i, GsonUtils.convertToT(mapNum, MapNumInfo.class));
        }
    }

}
