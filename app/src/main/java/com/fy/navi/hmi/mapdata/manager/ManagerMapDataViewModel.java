package com.fy.navi.hmi.mapdata.manager;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

public class ManagerMapDataViewModel extends BaseViewModel<ManagerMapDataFragment, ManagerMapDataModel> {
    public MutableLiveData<Boolean> mDownloadingNoDataVisibility = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mDownloadedNoDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mDownloadingDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mDownloadedDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<String> mAllDownloadingDataSize = new MutableLiveData<>("0");
    public MutableLiveData<String> mAllDownloadedDataSize = new MutableLiveData<>("0");

    private boolean mIsDownloadedPage = false;

    public ManagerMapDataViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected ManagerMapDataModel initModel() {
        return new ManagerMapDataModel();
    }

    /**
     * 返回上一页
     */
    public Action mBackMapDataView = () -> {
        closeFragment(true);
    };

    public Action mDownloadingDataClickView = () -> {
        mIsDownloadedPage = false;
        setDownloadingView(mModel.getWorkingList());
        mView.setDownloadingTitleStatus(true);
        mView.setDownloadedTitleStatus(false);
    };

    public Action mDownloadedClickView = () -> {
        mIsDownloadedPage = true;
        setDownloadedView(mModel.getWorkedList());
        mView.setDownloadingTitleStatus(false);
        mView.setDownloadedTitleStatus(true);
    };

    public Action mAllDataSuspend = () -> {
        mModel.pauseAllTask(mModel.getAllWorkingAdCodeList(mModel.getWorkingList()));
        mView.setStartAllTaskStatus(false);
        mView.setPauseAllTaskStatus(true);
    };

    public Action mAllDataDownload = () -> {
        mModel.startAllTask(mModel.getAllWorkingAdCodeList(mModel.getWorkingList()));
        mView.setStartAllTaskStatus(true);
        mView.setPauseAllTaskStatus(false);
    };

    /**
     * 初始化view
     */
    public void initView() {
        mModel.initView();
    }

    /**
     * 开始下载
     * @param adCodeList
     */
    public void startAllTask(final ArrayList<Integer> adCodeList) {
        mModel.startAllTask(adCodeList);
    }

    /**
     * 删除数据包
     * @param adCodeList
     */
    public void deleteAllTask(final ArrayList<Integer> adCodeList) {
        mModel.deleteAllTask(adCodeList);
    }

    /**
     * 暂停下载
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    /**
     * 获取已下载列表
     * @return 返回已下载信息
     */
    public ArrayList<CityDataInfo> getWorkedList() {
        return mModel.getWorkedList();
    }

    /**
     * 更新下载中数据view
     * @param cityDataInfos
     */
    public void setDownloadingView(final ArrayList<CityDataInfo> cityDataInfos) {
        if (mIsDownloadedPage) {
            return;
        }
        if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
            mDownloadingDataVisibility.setValue(true);
            mDownloadingNoDataVisibility.setValue(false);
            mDownloadedNoDataVisibility.setValue(false);
            mDownloadedDataVisibility.setValue(false);
            mView.updateDownloadingView(cityDataInfos);
            mAllDownloadingDataSize.setValue(String.valueOf(cityDataInfos.size()));
        } else {
            mDownloadingDataVisibility.setValue(false);
            mDownloadingNoDataVisibility.setValue(true);
            mDownloadedNoDataVisibility.setValue(false);
            mDownloadedDataVisibility.setValue(false);
        }
    }

    /**
     * 更新已下载数据view
     * @param cityDataInfos
     */
    public void setDownloadedView(final ArrayList<CityDataInfo> cityDataInfos) {
        if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
            mDownloadingDataVisibility.setValue(false);
            mDownloadingNoDataVisibility.setValue(false);
            mDownloadedNoDataVisibility.setValue(false);
            mDownloadedDataVisibility.setValue(true);
            mView.updateDownloadedView(cityDataInfos);
            mAllDownloadedDataSize.setValue(ResourceUtils.Companion.getInstance().getString(R.string.offline_manager_map_downloading_size_start)
                    + cityDataInfos.size() + ResourceUtils.Companion.getInstance().getString(R.string.offline_manager_map_downloading_size_end));
        } else {
            mDownloadingDataVisibility.setValue(false);
            mDownloadingNoDataVisibility.setValue(false);
            mDownloadedNoDataVisibility.setValue(true);
            mDownloadedDataVisibility.setValue(false);
        }
    }

}
