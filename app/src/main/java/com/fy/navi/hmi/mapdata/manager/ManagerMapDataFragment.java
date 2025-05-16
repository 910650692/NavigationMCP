package com.fy.navi.hmi.mapdata.manager;

import android.view.View;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentManagerMapDataBinding;
import com.fy.navi.hmi.mapdata.DownloadReminderDialog;
import com.fy.navi.hmi.mapdata.adapter.ManagerMapDataAdapter;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

public class ManagerMapDataFragment extends BaseFragment<FragmentManagerMapDataBinding, ManagerMapDataViewModel> {
    private ManagerMapDataAdapter mDownloadingMapDataAdapter;
    private ManagerMapDataAdapter mDownloadedMapDataAdapter;
    private boolean mIsDelete;
    private List<CityDataInfo> mAllDownloadingList = new ArrayList<>();
    private boolean mAllStartButtonChecked;
    private boolean mAllPauseButtonChecked;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_manager_map_data;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initDownloadMapDataView();
        mViewModel.initView();
    }

    @Override
    public void onInitData() {

    }

    /**
     * 初始化下载管理view
     */
    private void initDownloadMapDataView() {
        mDownloadingMapDataAdapter = new ManagerMapDataAdapter(getActivity());
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadingOffline.setLayoutManager(layoutManager);
        mBinding.rvDownloadingOffline.setItemAnimator(null);
        mBinding.rvDownloadingOffline.setAdapter(mDownloadingMapDataAdapter);
        mDownloadingMapDataAdapter.setOnChildClickListener(new ManagerMapDataAdapter.OnChildClickListener() {

            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "startAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                showDialog(false, cityAdCodes);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "pauseAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                showDialog(true, cityAdCodes);
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "deleteAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                if (mViewModel != null) {
                    mViewModel.deleteAllTask(cityAdCodes);
                }
            }

        });

        mDownloadedMapDataAdapter = new ManagerMapDataAdapter(getActivity());
        final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getActivity());
        layoutManager1.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadedOffline.setLayoutManager(layoutManager1);
        mBinding.rvDownloadedOffline.setAdapter(mDownloadedMapDataAdapter);
        mDownloadedMapDataAdapter.setOnChildClickListener(new ManagerMapDataAdapter.OnChildClickListener() {

            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "startAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "pauseAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "deleteAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mIsDelete = true;
                mViewModel.deleteAllTask(cityAdCodes);
            }

        });

    }

    /**
     * 设置下载中view数据
     * @param provDataInfos
     */
    public void updateDownloadingView(final ArrayList<ProvDataInfo> provDataInfos) {
        ThreadManager.getInstance().postUi(() -> {
            mDownloadingMapDataAdapter.setData(provDataInfos);
            if (!ConvertUtils.isEmpty(provDataInfos)) {
                mAllDownloadingList.clear();
                for (ProvDataInfo provDataInfo : provDataInfos) {
                    mAllDownloadingList.addAll(provDataInfo.getCityInfoList());
                }
                updateDownloadingButtonStatus();
            }
        });
    }

    /**
     * 更新数据列表下载进度&状态
     * @param parentId
     * @param childId
     * @param newValue
     */
    public void notifyDowningView(final int parentId, final int childId, final CityDownLoadInfo newValue) {
        mDownloadingMapDataAdapter.updateChild(parentId, childId, newValue);

        for (CityDataInfo cityDataInfo : mAllDownloadingList) {
            if (cityDataInfo.getAdcode() == childId) {
                cityDataInfo.getDownLoadInfo().setTaskState(newValue.getTaskState());
            }
        }
        updateDownloadingButtonStatus();
    }

    /**
     * 更新已下载view
     * @param provDataInfos
     * @param isChange
     */
    public void updateDownloadedView(final ArrayList<ProvDataInfo> provDataInfos) {
        mDownloadedMapDataAdapter.setData(provDataInfos);
    }

    /**
     * 更新按钮状态
     */
    private void updateDownloadingButtonStatus() {
        if (mAllDownloadingList.isEmpty()) {
            return;
        }
        mAllStartButtonChecked = false;
        mAllPauseButtonChecked = false;
        for (CityDataInfo cityDataInfo : mAllDownloadingList) {
            final int taskState = cityDataInfo.getDownLoadInfo().getTaskState();
            if (taskState == UserDataCode.TASK_STATUS_CODE_PAUSE
                || taskState == UserDataCode.TASK_STATUS_CODE_ERR) {
                mAllStartButtonChecked = true;
            }
            if (taskState == UserDataCode.TASK_STATUS_CODE_DOING
                || taskState == UserDataCode.TASK_STATUS_CODE_WAITING) {
                mAllPauseButtonChecked = true;
            }
        }
        setStartAllTaskStatus(mAllStartButtonChecked);
        setPauseAllTaskStatus(mAllPauseButtonChecked);
    }

    /**
     * 是否全部开始按钮高亮
     * @return boolean
     */
    public boolean getAllStartButtonChecked() {
        return mAllStartButtonChecked;
    }

    /**
     * 是否全部暂停高亮
     * @return boolean
     */
    public boolean getAllPauseButtonChecked() {
        return mAllPauseButtonChecked;
    }

    /**
     * 设置下载中全部开始/暂停状态
     * @param status
     */
    public void setDownloadingTitleStatus(final boolean status) {
        ThreadManager.getInstance().postUi(() -> {
            if (status) {
                mBinding.tvOfflineAllData.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_select));
                mBinding.offlineAllDataLine.setVisibility(View.VISIBLE);
            } else {
                mBinding.tvOfflineAllData.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_voice_text));
                mBinding.offlineAllDataLine.setVisibility(View.GONE);
            }
        });
    }

    /**
     * 设置已下载title状态
     * @param status
     */
    public void setDownloadedTitleStatus(final boolean status) {
        ThreadManager.getInstance().postUi(() -> {
            if (status) {
                mBinding.tvOfflineDownloadAdministration.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_select));
                mBinding.downloadAdministrationLine.setVisibility(View.VISIBLE);
            } else {
                mBinding.tvOfflineDownloadAdministration.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_voice_text));
                mBinding.downloadAdministrationLine.setVisibility(View.GONE);
            }
        });
    }

    /**
     * 设置下载中 - 全部开始 选中状态
     * @param isChecked
     */
    public void setStartAllTaskStatus(final boolean isChecked) {
        ThreadManager.getInstance().postUi(() -> {
            if (isChecked) {
                mBinding.allDataDownload.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_checkbox_select));
                mBinding.tvDownloadAllStart.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
                mBinding.imgDownloadAllStart.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_start_select));
            } else {
                mBinding.allDataDownload.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
                mBinding.tvDownloadAllStart.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
                mBinding.imgDownloadAllStart.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_download_all_start));
            }
        });
    }

    /**
     * 设置下载中 - 全部暂停 选中状态
     * @param isChecked
     */
    public void setPauseAllTaskStatus(final boolean isChecked) {
        ThreadManager.getInstance().postUi(() -> {
            if (isChecked) {
                mBinding.allDataSuspend.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_checkbox_select));
                mBinding.tvDownloadAllSuspend.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
                mBinding.imgDownloadAllSuspend.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_suspend_select));
            } else {
                mBinding.allDataSuspend.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
                mBinding.tvDownloadAllSuspend.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
                mBinding.imgDownloadAllSuspend.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_suspend));
            }
        });
    }

    /**
     * 显示弹框
     * @param isDownloading
     * @param cityAdCodes
     */
    private void showDialog(final boolean isDownloading, final ArrayList<Integer> cityAdCodes) {
        ThreadManager.getInstance().postUi(() -> {
            final DownloadReminderDialog downloadReminderDialog = new DownloadReminderDialog(mActivity);
            downloadReminderDialog.setContent(isDownloading);
            downloadReminderDialog.setOnDialogClickListener(new DownloadReminderDialog.OnDialogClickListener() {
                @Override
                public void onCommitClick() {
                    if (mViewModel != null) {
                        if (isDownloading) {
                            mViewModel.pauseAllTask(cityAdCodes);
                        } else {
                            mViewModel.startAllTask(cityAdCodes);
                        }
                    }
                }
                @Override
                public void onCancelClick() {
                    if (mViewModel != null) {
                        mViewModel.cancelAllTask(cityAdCodes);
                    }
                }
            });
            downloadReminderDialog.show();
        });
    }

}
