package com.sgm.navi.service.adapter.forcast;

import androidx.work.ListenableWorker;

import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.bean.FyOftenArrivedItem;

import java.util.ArrayList;

/**
 * @author lww
 * @date 2025/3/25
 */
public class ForCastAdapter {
    private static final String ENGINE_API_PKG = ForCastAdapter.class.getPackage().getName();
    private static final String ENGINE_API_CLS = "ForCastApiImpl";
    private final IForCastApi mIForCastApi;

    private ForCastAdapter() {
        mIForCastApi = (IForCastApi) AdapterConfig.getObject(ENGINE_API_PKG, ENGINE_API_CLS);
    }

    public static ForCastAdapter getInstance() {
        return Helper.fcAdapter;
    }

    public ListenableWorker.Result initForCastService() {
        return mIForCastApi.initForCastService();
    }

    public void registerCallback() {
        mIForCastApi.registerCallback();
    }

    public void unRegisterCallback() {
        mIForCastApi.unRegisterCallback();
    }

    public void destroyService() {
        mIForCastApi.destroyService();
    }

    public void onlineForCastArrive() {
        mIForCastApi.onlineForCastArrive();
    }

    public ArrayList<FyOftenArrivedItem> getArrivedDataList(int type) {
        return mIForCastApi.getArrivedDataList(type);
    }

    public void addArrivedDataList(int type, FyOftenArrivedItem arrivedItem) {
        mIForCastApi.addArrivedDataList(type, arrivedItem);
    }

    public void delLocalArrivedData(int type, String name) {
        mIForCastApi.delLocalArrivedData(type, name);
    }

    public ArrayList<FyOftenArrivedItem> getFrequentItemList() {
        return mIForCastApi.getFrequentItemList();
    }

    public void setFrequentItemList(ArrayList<FyOftenArrivedItem> data) {
        mIForCastApi.setFrequentItemList(data);
    }

    private static final class Helper {
        private static final ForCastAdapter fcAdapter = new ForCastAdapter();
    }
}
