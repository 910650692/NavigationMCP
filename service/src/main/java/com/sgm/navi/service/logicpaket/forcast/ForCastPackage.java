package com.sgm.navi.service.logicpaket.forcast;

import androidx.work.ListenableWorker;

import com.sgm.navi.service.adapter.forcast.ForCastAdapter;
import com.sgm.navi.service.define.bean.FyOftenArrivedItem;

import java.util.ArrayList;

/**
 * @author lww
 * @date 2025/3/26
 */
public class ForCastPackage {
    private ForCastAdapter mForCastAdapter;

    private ForCastPackage() {
        mForCastAdapter = ForCastAdapter.getInstance();
    }

    public static ForCastPackage getInstance() {
        return Helper.fcPackage;
    }

    public ListenableWorker.Result initForCastService() {
        return mForCastAdapter.initForCastService();
    }

    public void registerCallback() {
        mForCastAdapter.registerCallback();
    }

    public void unRegisterCallback() {
        mForCastAdapter.unRegisterCallback();
    }

    public void destroyService() {
        mForCastAdapter.destroyService();
    }

    /*** 开启在线预测 **/
    public void onlineForCastArrive() {
        mForCastAdapter.onlineForCastArrive();
    }

    /*** 获取常去地点列表 **/
    public ArrayList<FyOftenArrivedItem> getArrivedDataList(int type) {
        return mForCastAdapter.getArrivedDataList(type);
    }

    /*** 添加常去地点 **/
    public void addArrivedDataList(int type, FyOftenArrivedItem arrivedItem) {
        mForCastAdapter.addArrivedDataList(type, arrivedItem);
    }

    /*** 删除常去地点 **/
    void delLocalArrivedData(int type, String name) {
        mForCastAdapter.delLocalArrivedData(type, name);
    }

    /*** 获取同步库常去地点列表 **/
    public ArrayList<FyOftenArrivedItem> getFrequentItemList() {
        return mForCastAdapter.getFrequentItemList();
    }

    /*** 保存同步库常去地点列表 **/
    public void setFrequentItemList(ArrayList<FyOftenArrivedItem> data) {
        mForCastAdapter.setFrequentItemList(data);
    }

    private static final class Helper {
        private static final ForCastPackage fcPackage = new ForCastPackage();
    }
}
