package com.fy.navi.hmi.mapdata;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.mapdata.manager.ManagerMapDataFragment;
import com.fy.navi.hmi.mapdata.near.NearMapDataFragment;
import com.fy.navi.hmi.mapdata.search.SearchMapDataFragment;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.math.BigInteger;
import java.util.ArrayList;

public class MapDataViewModel extends BaseViewModel<MapDataFragment, MapDataModel> {
    public MutableLiveData<String> mAllDownloadingDataSize = new MutableLiveData<>();
    public MutableLiveData<Boolean> mNearDownloadBtnVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mManagerDownloadVisibility = new MutableLiveData<>(false);
    public MutableLiveData<String> mNearCityDataSize = new MutableLiveData<>("0");

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
                mAllDownloadingDataSize.setValue(String.valueOf(mModel.getWorkingQueueSize()));
                //获取全部地图初始化数据
                mView.updateMapDataView(mModel.getMapDataList());
                // 获取当前城市数据
                mView.updateCurrentCityView(mModel.getCurrentCityInfo());
                // 获取基础功能包数据
                mView.updateCountryDataView(mModel.getCountryData());
                // 获取附近推荐城市信息
                updateNearDataView();
                // 发起云端数据列表检测
                mModel.requestDataListCheck(isCheck);
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
            final int adCode = adCodeList.get(0);
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
            notifyNearDataView();
            mAllDownloadingDataSize.setValue(String.valueOf(mModel.getWorkingQueueSize()));
        });
    }

    public CityDataInfo getCurrentCityInfo() {
        return  mModel.getCurrentCityInfo();
    }

    public CityDataInfo getCountryData() {
        return  mModel.getCountryData();
    }

    /**
     * 更新下载管理界面状态
     */
    public void updateManagerDownloadView() {
        final ArrayList<ProvDataInfo> workingList = mModel.getWorkingList();
        if (!ConvertUtils.isEmpty(workingList)) {
            mManagerDownloadVisibility.setValue(true);
        } else {
            final ArrayList<ProvDataInfo> allDownLoadedList = mModel.getAllDownLoadedList();
            mManagerDownloadVisibility.setValue(!ConvertUtils.isEmpty(allDownLoadedList));
        }
    }

    /**
     * 更新附近城市推荐信息
     */
    private void updateNearDataView() {
        final ArrayList<CityDataInfo> nearCityList = mModel.getNearAdCodeList();
        if (ConvertUtils.isEmpty(nearCityList)) {
            return;
        }
        BigInteger sum  = BigInteger.ZERO;
        final int count = nearCityList.size();
        int downloadedCount = 0;
        for (CityDataInfo info : nearCityList) {
            //获取附近城市数据包大小总和
            sum =  sum.add(info.getDownLoadInfo().getFullZipSize());
            //获取附近城市未下载数量
            if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY) {
                downloadedCount++;
            }
        }
        mNearCityDataSize.setValue(count + "个城市  共" + StringUtils.formatSize(sum));
        mNearDownloadBtnVisibility.setValue(downloadedCount == count);
    }

    /**
     * 刷新附近城市推荐 - 全部下载按显隐状态
     */
    private void notifyNearDataView() {
        final ArrayList<CityDataInfo> nearList = mModel.getNearAdCodeList();
        if (!ConvertUtils.isEmpty(nearList)) {
            final long downloadedCount = nearList.stream()
                .filter(cityDataInfo -> cityDataInfo.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY)
                .count();
            mNearDownloadBtnVisibility.setValue(downloadedCount == nearList.size());
        }
    }
}
