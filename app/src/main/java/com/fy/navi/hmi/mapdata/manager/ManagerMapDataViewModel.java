package com.fy.navi.hmi.mapdata.manager;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;
import java.util.List;

public class ManagerMapDataViewModel extends BaseViewModel<ManagerMapDataFragment, ManagerMapDataModel> {
    public MutableLiveData<Boolean> mDownloadingNoDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mDownloadedNoDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mDownloadingDataVisibility = new MutableLiveData<>(true);
    public MutableLiveData<Boolean> mDownloadedDataVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Integer> mAllDownloadingDataSize = new MutableLiveData<>(0);
    public MutableLiveData<String> mAllDownloadedDataSize = new MutableLiveData<>("0");

    private boolean mIsDownloadedPage = false;
    private ArrayList<ProvDataInfo> downloadingInfos = new ArrayList<>();

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

    /**
     * 正在下载tab
     */
    public Action mDownloadingDataClickView = () -> {
        mIsDownloadedPage = false;
        mView.setDownloadingTitleStatus(true);
        mView.setDownloadedTitleStatus(false);
        mDownloadingDataVisibility.setValue(true);
        mDownloadedNoDataVisibility.setValue(false);
        mDownloadedDataVisibility.setValue(false);
        setDownloadingView(downloadingInfos);
    };

    /**
     * 已下载tab
     */
    public Action mDownloadedClickView = () -> {
        mIsDownloadedPage = true;
        mView.setDownloadingTitleStatus(false);
        mView.setDownloadedTitleStatus(true);
        mDownloadedDataVisibility.setValue(true);
        mDownloadingDataVisibility.setValue(false);
        mDownloadingNoDataVisibility.setValue(false);
        setDownloadedView(mModel.getWorkedList(), false);
    };

    public Action mAllDataSuspend = () -> {
        if (!mView.getAllPauseButtonChecked()) {
            return;
        }
        ArrayList<Integer> adCodeList = new ArrayList<>();
        if (downloadingInfos != null && !downloadingInfos.isEmpty()) {
            for (int i = 0; i < downloadingInfos.size(); i++) {
                final List<CityDataInfo> citys = downloadingInfos.get(i).getCityInfoList();
                for (CityDataInfo cityDataInfo : citys) {
                    adCodeList.add(cityDataInfo.getAdcode());
                }
            }
        }
        mModel.pauseAllTask(adCodeList);
    };

    public Action mAllDataDownload = () -> {
        if (!mView.getAllStartButtonChecked()) {
            return;
        }
        ArrayList<Integer> adCodeList = new ArrayList<>();
        if (downloadingInfos != null && !downloadingInfos.isEmpty()) {
            for (int i = 0; i < downloadingInfos.size(); i++) {
                final List<CityDataInfo> citys = downloadingInfos.get(i).getCityInfoList();
                for (CityDataInfo cityDataInfo : citys) {
                    adCodeList.add(cityDataInfo.getAdcode());
                }
            }
        }
        mModel.startAllTask(adCodeList);
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
     * 取消下载
     * @param adCodeList
     */
    public void cancelAllTask(final ArrayList<Integer> adCodeList) {
        mModel.cancelAllTask(adCodeList);
    }

    /**
     * 暂停下载
     * @param adCodeList
     */
    public void pauseAllTask(final ArrayList<Integer> adCodeList) {
        mModel.pauseAllTask(adCodeList);
    }

    /**
     * 更新下载中数据view
     * @param provDataInfos
     */
    public void setDownloadingView(final ArrayList<ProvDataInfo> provDataInfos) {
        downloadingInfos = provDataInfos;
        if (mIsDownloadedPage) {
            return;
        }
        int size = 0;
        if (provDataInfos != null && !provDataInfos.isEmpty()) {
            for (int i = 0; i < provDataInfos.size(); i++) {
                final List<CityDataInfo> city = provDataInfos.get(i).getCityInfoList();
                size = city.size() + size;
            }
        }
        mDownloadingNoDataVisibility.setValue(size == 0);
        mAllDownloadingDataSize.setValue(size);
        mView.updateDownloadingView(provDataInfos);
    }

    /**
     * 更新已下载数据view
     * @param provDataInfos
     */
    public void setDownloadedView(final ArrayList<ProvDataInfo> provDataInfos, boolean isChange) {
        int size = 0;
        if (provDataInfos != null && !provDataInfos.isEmpty()) {
            for (int i = 0; i < provDataInfos.size(); i++) {
                final List<CityDataInfo> city = provDataInfos.get(i).getCityInfoList();
                size = city.size() + size;
            }
        }

        mDownloadedNoDataVisibility.setValue(size == 0);
        mView.updateDownloadedView(provDataInfos, isChange);
        mAllDownloadedDataSize.setValue(ResourceUtils.Companion.getInstance().getString(R.string.offline_manager_map_downloading_size_start)
            + size + ResourceUtils.Companion.getInstance().getString(R.string.offline_manager_map_downloading_size_end));

    }

    /**
     * 实时更新下载状态
     * @param info
     */
    public void onDownLoadStatus(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {

            if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS
                || info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY) {
                // 取消下载or已下载，重新加载下载中列表信息
                setDownloadingView(mModel.getWorkingList());
                //当前处于已下载tab页，实时删除数据包，会动态刷新当前数据
                if (mIsDownloadedPage) {
                    setDownloadedView(mModel.getWorkedList(), true);
                }
            } else {
                //实时更新列表item
                mView.notifyDowningView(info.getUpperAdcode(), info.getAdcode(), info.getDownLoadInfo());
            }

        });
    }

}
