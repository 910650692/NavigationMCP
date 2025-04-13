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
    public MutableLiveData<Boolean> mNearDownloadBtnVisibility  = new MutableLiveData<>(false);

    public MapDataViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected MapDataModel initModel() {
        return new MapDataModel();
    }

    /**
     * 获取全部省份+城市数据
     * @param isCheck
     */
    public void getAllProvinceData(final boolean isCheck) {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                mAllDownloadingDataSize.setValue(String.valueOf(mModel.getWorkingQueueSize()));
                //获取全部地图初始化数据
                mView.updateMapDataView(mModel.getMapDataList());
                // 获取当前城市数据
                mView.updateCurrentCityView(mModel.getCurrentCityInfo());
                // 获取基础功能包数据
                mView.updateCountryDataView(mModel.getCountryData());
                // 获取下载中、更新中状态下的所有城市adCode列表数据
                mView.updateWorkingView(mModel.getWorkingList(), mModel.getAllDownLoadedList());
                // 获取附近推荐城市信息
                mView.updateNearDataView(mModel.getNearAdCodeList());
                // 发起云端数据列表检测
                mModel.requestDataListCheck(isCheck);
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
        ThreadManager.getInstance().postDelay(() -> {
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
        }, 0);
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
        ThreadManager.getInstance().postDelay(() -> {
            final CityDataInfo info = mModel.getCurrentCityInfo();
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
        }, 0);
    };

    /**
     * 附近城市-全部下载
     */
    public Action mToDownloadNearData = () -> {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                mNearDownloadBtnVisibility.setValue(false);// 隐藏全部下载按钮
                final ArrayList<CityDataInfo> list = mModel.getNearAdCodeList();
                final ArrayList<Integer> cityAdcodes = new ArrayList<>();
                for(CityDataInfo info : list) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY
                            || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE
                            || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR
                            || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX) {
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
        if (adCodeList != null && !adCodeList.isEmpty()) {
            int adCode = adCodeList.get(0);
            if (mModel.countryDataVisible() && adCode != 0) {
                // 初次下载“非基础功能包”的任意城市包，始终提示此弹窗
                mView.showCountryMapDataDialog();
            }
            mModel.startAllTask(adCodeList);
        }
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
     * @param cityDataInfo
     */
    public void onDownLoadStatus(final CityDataInfo cityDataInfo) {
        ThreadManager.getInstance().postUi(() -> {
            mView.notifyMapDataChangeView(cityDataInfo);
            mView.updateCountryDataView(cityDataInfo);
            mView.notifyCurrentCityView(cityDataInfo);
            mView.notifyNearDataView();
            mAllDownloadingDataSize.setValue(String.valueOf(mModel.getWorkingQueueSize()));
        });
    }

    public CityDataInfo getCurrentCityInfo() {
        return  mModel.getCurrentCityInfo();
    }

    public CityDataInfo getCountryData() {
        return  mModel.getCountryData();
    }

    public  ArrayList<CityDataInfo> getNearCityData() {
        return  mModel.getNearAdCodeList();
    }

}
