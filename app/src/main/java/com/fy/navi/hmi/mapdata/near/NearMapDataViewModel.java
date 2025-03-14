package com.fy.navi.hmi.mapdata.near;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2025/03/13
 */
public class NearMapDataViewModel extends BaseViewModel<NearMapDataFragment, NearMapDataModel> {

    public NearMapDataViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected NearMapDataModel initModel() {
        return new NearMapDataModel();
    }


    public void initData() {
        mModel.initData();
    }

    public void setNearCityInfo(ArrayList<CityDataInfo> cityDataInfos) {
        mView.updateNearView(cityDataInfos);
    }
    /**
     * 返回上一页
     */
    public Action closeNearMapDataView = () -> {
        closeFragment(true);
    };


    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void startAllTask(ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * 暂停正在下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void pauseAllTask(ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    public void cancelAllTask(ArrayList<Integer> adCodeList) {
        mModel.cancelAllTask(adCodeList);
    }



}
