package com.fy.navi.hmi.mapdata.near;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class NearMapDataViewModel extends BaseViewModel<NearMapDataFragment, NearMapDataModel> {

    public NearMapDataViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected NearMapDataModel initModel() {
        return new NearMapDataModel();
    }

    /**
     * 初始化页面数据
     */
    public void initData() {
        mModel.initData();
    }

    /**
     * 更新附近推荐城市view
     * @param cityDataInfos
     */
    public void setNearCityInfo(final ArrayList<CityDataInfo> cityDataInfos) {
        mView.updateNearView(cityDataInfos);
    }

    /**
     * 返回上一页
     */
    public Action mCloseNearMapDataView = () -> {
        closeFragment(true);
    };


    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mModel.deleteAllTask(adCodeList);
    }

    /**
     * 暂停正在下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

}
