package com.fy.navi.service.adapter.forcast;

import androidx.work.ListenableWorker;

import com.fy.navi.service.define.bean.FyOftenArrivedItem;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/25
 */
public interface IForCastApi {
    /**
     * 通勤服务init
     * @return init result
     */
    ListenableWorker.Result initForCastService();

    /**
     * registerCallback
     */
    void registerCallback();

    /**
     * unRegisterCallback
     */
    void unRegisterCallback();

    /**
     * 通勤服务销毁
     */
    void destroyService();

    /**
     * 在线通勤预测逻辑，依赖网络和账号登录
     */
    void onlineForCastArrive();

    /**
     * 获取常去地点列表
     */
    ArrayList<FyOftenArrivedItem> getArrivedDataList(int type);

    /**
     * 添加常去地点
     */
    void addArrivedDataList(int type, FyOftenArrivedItem arrivedItem);

    /**
     * 删除常去地点
     */
    void delLocalArrivedData(int type, String name);

    /**
     * 获取同步库常去地点列表
     */
    ArrayList<FyOftenArrivedItem> getFrequentItemList();

    /**
     * 保存同步库常去地点列表
     */
    void setFrequentItemList(ArrayList<FyOftenArrivedItem> data);
}
