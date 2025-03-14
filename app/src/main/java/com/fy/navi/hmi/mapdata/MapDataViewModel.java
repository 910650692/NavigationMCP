package com.fy.navi.hmi.mapdata;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.mapdata.manager.ManagerMapDataFragment;
import com.fy.navi.hmi.mapdata.near.NearMapDataFragment;
import com.fy.navi.hmi.mapdata.search.SearchMapDataFragment;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataViewModel extends BaseViewModel<MapDataFragment, MapDataModel> {
    private static final String TAG = MapDataViewModel.class.getName();
    public int managerType;

    public MapDataViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected MapDataModel initModel() {
        return new MapDataModel();
    }

    /**
     * 获取全部省份+城市数据
     */
    public void getAllProvinceData() {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                //获取全部地图初始化数据
                mView.updateMapDataView(mModel.getMapDataList());
                // 获取当前城市数据
                mView.updateCurrentCityView(mModel.getCurrentCityInfo(310000));
                // 获取基础功能包数据
                mView.updateCountryDataView(mModel.getCountryData());
                // 获取下载中、更新中状态下的所有城市adCode列表数据
                mView.updateWorkingView(mModel.getWorkingList());
                // 获取已下载状态下的所有城市adCode列表数据
                mView.updateWorkedView(mModel.getWorkedList());
                // 获取附近推荐城市信息
                mView.updateNearDataView(mModel.getNearAdCodeList(310000));
            }
        }, 0);
    }

    public Action finishMapDataView = () -> closeFragment(true);

    /**
     * 搜索地图数据
     */
    public Action searchMapDataView = () -> {
        addFragment(new SearchMapDataFragment(), null);
    };

    /**
     * 跳转到附近城市推荐页面
     */
    public Action toNearMapDataView = () -> {
        addFragment(new NearMapDataFragment(), null);
    };

    /**
     * 跳转到下载管理页面
     */
    public Action toManagerMapDataView = () -> {
        addFragment(new ManagerMapDataFragment(), null);
    };

    /**
     * 下载基础包
     */
    public Action startDownload = () -> {
        CityDataInfo info = mModel.getCountryData();
        ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(info.adcode);
        startAllTask(cityAdcodes);
    };

    /**
     * 下载当前城市
     */
    public Action toDownloadCurrentCity = () -> {
        CityDataInfo info = mModel.getCurrentCityInfo(310000);
        ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(info.adcode);
        startAllTask(cityAdcodes);
    };


    /**
     * 全部地图
     */
    public Action allDataClickView = () -> {
//        mView.updateMapDataTab(1);
    };

    /**
     * 下载管理
     */
    public Action administrationClickView = () -> {
//        mView.updateMapDataTab(2);
    };

    /**
     * 全部开始
     */
    public Action allDataDownload = () -> {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                managerType = 0;
                ArrayList<CityDataInfo> list = mModel.getWorkingList();
                ArrayList<Integer> cityAdcodes = new ArrayList<>();
                for(CityDataInfo info :list) {
                    if (info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_WAITING  // 等待中
                            || info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_DOING
                            || info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_DONE) { // 下载中
                        cityAdcodes.add(info.adcode);
                    }
                }
                startAllTask(cityAdcodes);
            }
        }, 0);
    };

    /**
     * 全部暂停
     */
    public Action allDataSuspend = () -> {
        ThreadManager.getInstance().postDelay(new Runnable() {
            @Override
            public void run() {
                managerType = 0;
                ArrayList<CityDataInfo> list = mModel.getWorkingList();
                ArrayList<Integer> cityAdcodes = new ArrayList<>();
                for(CityDataInfo info :list) {
                    if (info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_PAUSE) { // 暂停
                        cityAdcodes.add(info.adcode);
                    }
                }
                pauseAllTask(cityAdcodes);
            }
        }, 0);
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

    /**
     * 删除已下载的城市数据
     * @param adCodeList 省份、城市ID列表
     */
    public void deleteAllTask(int type, ArrayList<Integer> adCodeList) {
        managerType = type;
        mModel.deleteAllTask(adCodeList);
    }

    public void onPercent(ProvDataInfo info) {
        mView.notifyMapDataChangeView(info);
               /* if () {
                    // 刷新基础包
                    mView.updateCountryDataView(info.cityInfoList.get(0));
                } else if () {
                    mView.updateCurrentCityView(info.cityInfoList.get(0));
                }*/
    }

    public void onDownLoadStatus(ProvDataInfo provDataInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (managerType == 0) {
            } else if (managerType == 1) {
//                mView.updateMapDataView(mModel.getMapDataList());
            } else {
//                mView.notifyMapDataChangeView(provDataInfo);
            }

            mView.updateWorkingView(mModel.getWorkingList());
            mView.updateWorkedView(mModel.getWorkedList());

        });
    }

}
