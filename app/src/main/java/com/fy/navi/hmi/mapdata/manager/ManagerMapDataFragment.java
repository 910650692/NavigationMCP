package com.fy.navi.hmi.mapdata.manager;

import android.view.View;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentManagerMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.CityMapDataAdapter;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

public class ManagerMapDataFragment extends BaseFragment<FragmentManagerMapDataBinding, ManagerMapDataViewModel> {
    private CityMapDataAdapter mDownloadingMapDataAdapter;
    private CityMapDataAdapter mDownloadedMapDataAdapter;

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
        mDownloadingMapDataAdapter = new CityMapDataAdapter(getActivity());
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadingOffline.setLayoutManager(layoutManager);
        mBinding.rvDownloadingOffline.setAdapter(mDownloadingMapDataAdapter);
        mDownloadingMapDataAdapter.setItemClickListener(new CityMapDataAdapter.OnItemClickListener() {

            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "startAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.startAllTask(cityAdCodes);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "pauseAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.pauseAllTask(cityAdCodes);
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "deleteAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                //mViewModel.deleteAllTask(cityAdCodes);
            }

        });

        mDownloadedMapDataAdapter = new CityMapDataAdapter(getActivity());
        final LinearLayoutManager layoutManager1 = new LinearLayoutManager(getActivity());
        layoutManager1.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadedOffline.setLayoutManager(layoutManager1);
        mBinding.rvDownloadedOffline.setAdapter(mDownloadedMapDataAdapter);
        mDownloadedMapDataAdapter.setItemClickListener(new CityMapDataAdapter.OnItemClickListener() {

            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "startAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.startAllTask(cityAdCodes);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "pauseAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.pauseAllTask(cityAdCodes);
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d( "deleteAllTask cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.deleteAllTask(cityAdCodes);
                mViewModel.setDownloadedView(mViewModel.getWorkedList());
            }

        });

    }

    /**
     * 更新下载中view
     * @param cityDataInfos
     */
    public void updateDownloadingView(final ArrayList<CityDataInfo> cityDataInfos) {

        ThreadManager.getInstance().postUi(() -> {
            if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
                mDownloadingMapDataAdapter.setData(cityDataInfos);
            }
        });
    }

    /**
     * 更新已下载view
     * @param cityDataInfos
     */
    public void updateDownloadedView(final ArrayList<CityDataInfo> cityDataInfos) {

        ThreadManager.getInstance().postUi(() -> {
            if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
                mDownloadedMapDataAdapter.setData(cityDataInfos);
            }
        });
    }

    /**
     * 设置下载中全部开始/暂停状态
     * @param status
     */
    public void setDownloadingTitleStatus(final boolean status) {
        ThreadManager.getInstance().postUi(() -> {
            if (status) {
                mBinding.tvOfflineAllData.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.black));
                mBinding.offlineAllDataLine.setVisibility(View.VISIBLE);
            } else {
                mBinding.tvOfflineAllData.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_70_000000));
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
                mBinding.tvOfflineDownloadAdministration.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.black));
                mBinding.downloadAdministrationLine.setVisibility(View.VISIBLE);
            } else {
                mBinding.tvOfflineDownloadAdministration.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.color_70_000000));
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
                mBinding.tvDownloadAllStart.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                mBinding.imgDownloadAllStart.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_start_select));
            } else {
                mBinding.allDataDownload.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
                mBinding.tvDownloadAllStart.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
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
                mBinding.tvDownloadAllSuspend.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                mBinding.imgDownloadAllSuspend.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_suspend_select));
            } else {
                mBinding.allDataSuspend.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
                mBinding.tvDownloadAllSuspend.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
                mBinding.imgDownloadAllSuspend.setImageDrawable(ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_download_all_suspend));
            }
        });
    }

}
