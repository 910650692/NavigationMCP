package com.fy.navi.hmi.mapdata;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.mapdata.manager.ManagerMapDataFragment;
import com.fy.navi.hmi.mapdata.near.NearMapDataFragment;
import com.fy.navi.hmi.mapdata.search.SearchMapDataFragment;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class MapDataViewModel extends BaseViewModel<MapDataFragment, MapDataModel> {
    public MutableLiveData<String> mAllDownloadingDataSize = new MutableLiveData<>("0");

    public MapDataViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected MapDataModel initModel() {
        return new MapDataModel();
    }

    /**
     * 获取全部省份+城市数据
     * @param ischeck
     */
    public void getAllProvinceData(final boolean ischeck) {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                mAllDownloadingDataSize.setValue(String.valueOf(mModel.getWorkingList().size()));
                //获取全部地图初始化数据
                mView.updateMapDataView(mModel.getMapDataList());
                // 获取当前城市数据
                mView.updateCurrentCityView(mModel.getCurrentCityInfo(310000));
                // 获取基础功能包数据
                mView.updateCountryDataView(mModel.getCountryData());
                // 获取下载中、更新中状态下的所有城市adCode列表数据
                mView.updateWorkingView(mModel.getWorkingList(), mModel.getWorkedList());
                // 获取附近推荐城市信息
                mView.updateNearDataView(mModel.getNearAdCodeList(310000));
                // 发起云端数据列表检测
                mModel.requestDataListCheck(ischeck);

            }
        }, 0);
    }

    public Action mFinishMapDataView = () -> closeFragment(true);

    /**
     * 搜索地图数据
     */
    public Action mSearchMapDataView = () -> {
        addFragment(new SearchMapDataFragment(), null);
    };

    /**
     * 跳转到附近城市推荐页面
     */
    public Action mToNearMapDataView = () -> {
        addFragment(new NearMapDataFragment(), null);
    };

    /**
     * 跳转到下载管理页面
     */
    public Action mToManagerMapDataView = () -> {
        addFragment(new ManagerMapDataFragment(), null);
    };

    /**
     * 下载基础包
     */
    public Action mStartDownload = () -> {
        final CityDataInfo info = mModel.getCountryData();
        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(info.getAdcode());
        final CityDownLoadInfo downloadItem = info.getDownLoadInfo();
        if (downloadItem != null) {
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                pauseAllTask(cityAdcodes);
            } else if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX) {
                startAllTask(cityAdcodes);
            }
        }
    };

    /**
     * 删除基础包
     */
    public Action mToDeleteCountryCity = () -> {
        final CityDataInfo info = mModel.getCountryData();
        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(info.getAdcode());
        deleteAllTask(cityAdcodes);
    };

    /**
     * 下载当前城市
     */
    public Action mToDownloadCurrentCity = () -> {
       /* if (mModel.countryDataVisible()) {
            mView.showCountryMapDataDialog();
        }*/
        final CityDataInfo info = mModel.getCurrentCityInfo(310000);
        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(info.getAdcode());
        final CityDownLoadInfo downloadItem = info.getDownLoadInfo();
        if (downloadItem != null) {
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                pauseAllTask(cityAdcodes);
            } else if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                    downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX) {
                startAllTask(cityAdcodes);
            }
        }
    };

    /**
     * 附近城市-全部下载
     */
    public Action mToDownloadNearData = () -> {
        /*if (mModel.countryDataVisible()) {
            mView.showCountryMapDataDialog();
        }*/
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                final ArrayList<CityDataInfo> list = mModel.getNearAdCodeList(310000);
                final ArrayList<Integer> cityAdcodes = new ArrayList<>();
                for(CityDataInfo info :list) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING  // 等待中
                            || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING
                            || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE) { // 下载中
                        cityAdcodes.add(info.getAdcode());
                    }
                }
                startAllTask(cityAdcodes);
            }
        }, 0);
    };

    /**
     * @param adCodeList 省份、城市ID列表
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * 暂停正在下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    /**
     * 取消下载
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mModel.cancelAllTask(adCodeList);
    }

    /**
     * 删除已下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mModel.deleteAllTask(adCodeList);
    }

    /**
     * 实时更新下载状态
     * @param provDataInfo
     */
    public void onDownLoadStatus(final ProvDataInfo provDataInfo) {
        mView.notifyMapDataChangeView(provDataInfo);
        mView.notifyCurrentCityView(provDataInfo.getCityInfoList().get(0));
        mView.updateCountryDataView(provDataInfo.getCityInfoList().get(0));
    }

    public CityDataInfo getCurrentCityInfo() {
        return  mModel.getCurrentCityInfo(310000);
    }

    public CityDataInfo getCountryData() {
        return  mModel.getCountryData();
    }
}
