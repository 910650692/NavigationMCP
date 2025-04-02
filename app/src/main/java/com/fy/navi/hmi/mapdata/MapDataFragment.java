package com.fy.navi.hmi.mapdata;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;

import androidx.recyclerview.widget.GridLayoutManager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.MapDataAdapter;
import com.fy.navi.hmi.mapdata.adapter.MuliteRecycleAdapter;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class MapDataFragment extends BaseFragment<FragmentMapDataBinding, MapDataViewModel> {
    private static final String TAG = MapDataFragment.class.getName();
    private GridLayoutManager manager;
    private MapDataAdapter mapDataAdapter;
    private final List<MuliteRecycleAdapter.DataTree<String, String>> mDataList = new ArrayList<>();
    private DownloadCountryDialog mDownloadCountryDialog;
    private boolean mIsCheck = false;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_map_data;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        // 初始化全部地图列表
        initMapDataView();
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if(bundle != null){
            mIsCheck = bundle.getBoolean("isCheck", false);
        } else {
            Logger.e("bundle is null");
        }
        if (mViewModel != null) {
            mViewModel.getAllProvinceData(mIsCheck);
        }
    }

    /**
     * 初始化离线view
     */
    private void initMapDataView() {
        mapDataAdapter = new MapDataAdapter(getActivity());
        mBinding.rvOffline.setItemAnimator(null);
        mBinding.rvOffline.setAdapter(mapDataAdapter);

        //以下是对布局进行控制，让省份占一行，城市占两列，效果相当于一个listView嵌套gridView的效果
         manager = new GridLayoutManager(getActivity(),1);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(final int position) {
                return 1;
                /*return mapDataAdapter.getItemViewType(position)
                        == MuliteRecycleAdapter.ItemStatus.VIEW_TYPE_GROUP_ITEM ? 2 : 1;*/
            }
        });
        manager.setRecycleChildrenOnDetach(true); // 可选，用于提升性能
        mBinding.rvOffline.setLayoutManager(manager);

        mapDataAdapter.setOfflineItemListener(new MapDataAdapter.OfflineItemListener() {
            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "startAllTask");
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.startAllTask(cityAdCodes);
                    }
                }, 0);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "pauseAllTask");
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.pauseAllTask(cityAdCodes);
                    }
                }, 0);
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "deleteAllTask");
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.deleteAllTask(cityAdCodes);
                    }
                }, 0);
            }

            @Override
            public void cancelAllTask() {
                Logger.d(TAG, "cancelAllTask");
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.cancelAllTask(null);
                    }
                }, 0);
            }
        });
    }

    /**
     * 显示全部省份+城市信息
     * @param provinceBeans
     */
    public void updateMapDataView(final List<ProvDataInfo> provinceBeans) {
        ThreadManager.getInstance().postUi(() -> {
            mDataList.clear();
            if (provinceBeans != null && !provinceBeans.isEmpty()) {
                for (int i = 0; i < provinceBeans.size(); i++) {
                    final List<CityDataInfo> city = provinceBeans.get(i).getCityInfoList();
                    mDataList.add(new MuliteRecycleAdapter.DataTree<>(provinceBeans.get(i).getName(), city));
                }
                mapDataAdapter.setData(mDataList);
            }
        });
    }

    /**
     * 更新数据列表下载进度&状态
     * @param info
     */
    public void notifyMapDataChangeView(final ProvDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
           final CityDataInfo cityDataInfo = info.getCityInfoList().get(0);
            Logger.d(TAG, "notifyMapDataChangeView  cityDataInfo = " + GsonUtils.toJson(cityDataInfo));
            for (int i = 0; i < mapDataAdapter.getData().size(); i++) {
                for (int j = 0; j < mapDataAdapter.getSubItem(i).size(); j++) {
                    if (mapDataAdapter.getSubItem(i).get(j).getAdcode() == cityDataInfo.getAdcode()) {
                        Logger.d(TAG, "notifyMapDataPercent  cityDataInfo.adcode = " + cityDataInfo.getAdcode());
                        mapDataAdapter.getSubItem(i).set(j, cityDataInfo);
                        mapDataAdapter.notifyDataSetChanged();
                        break;
                    }
                }
            }
        });
    }

    /**
     * 显示当前城市信息
     * @param info
     */
    @SuppressLint("SetTextI18n")
    public void updateCurrentCityView(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            if (info != null && info.getDownLoadInfo() != null) {
                final String sizeString = StringUtils.formatSize(info.getDownLoadInfo().getFullZipSize());
                mBinding.currentCityData.setText(info.getName() + "   " + sizeString);
                mBinding.downloadView.parseDownloadStatusInfo(info.getDownLoadInfo());
            }
        });
    }

    /**
     * 更新当前城市下载按钮状态
     * @param info
     */
    public void notifyCurrentCityView(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            final CityDataInfo currentInfo = mViewModel.getCurrentCityInfo();
            if (info.getAdcode() == currentInfo.getAdcode()) {
                mBinding.downloadView.parseDownloadStatusInfo(info.getDownLoadInfo());
            }
        });
    }

    /**
     * 显示基础功能包信息
     * @param info
     */
    public void updateCountryDataView(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            final CityDataInfo countryInfo = mViewModel.getCountryData();
            if (info.getAdcode() == countryInfo.getAdcode()) {
                if (info.getDownLoadInfo() != null) {
                    //城市下载状态
                    //非已下载状态，禁止侧滑删除
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                        mBinding.swipeMenuLayout.setSwipeEnabled(true);
                    } else {
                        mBinding.swipeMenuLayout.setSwipeEnabled(false);
                        mBinding.swipeMenuLayout.smoothClose();
                    }

                    mBinding.countryCityName.setText(info.getName());
                    //城市数据包大小
                    final String sizeString = StringUtils.formatSize(info.getDownLoadInfo().getFullZipSize());
                    mBinding.countryDataCount.setText(sizeString);
                    // 下载按钮状态
                    mBinding.countryDownloadView.parseDownloadStatusInfo(info.getDownLoadInfo());
                }
            }
        });
    }

    /**
     * * 是否显示下载管理view
     * @param downloadingList
     * @param downloadedList
     */
    public void updateWorkingView(final ArrayList<CityDataInfo> downloadingList,
                                  final ArrayList<CityDataInfo> downloadedList) {
        ThreadManager.getInstance().postUi(() -> {
            if ((downloadingList != null && !downloadingList.isEmpty())
                    || (downloadedList!= null &&!downloadedList.isEmpty())) {
                mBinding.managerDataView.setVisibility(View.VISIBLE);
            } else {
                mBinding.managerDataView.setVisibility(View.GONE);
            }
        });
    }

    /**
     * 显示附近城市推荐信息
     * @param nearList
     */
    @SuppressLint("SetTextI18n")
    public void updateNearDataView(final ArrayList<CityDataInfo> nearList) {
        ThreadManager.getInstance().postUi(() -> {
            if (nearList != null && !nearList.isEmpty()) {
                BigInteger sum  = BigInteger.ZERO;
                final int count = nearList.size();
                int downloadedCount = 0;
                for (CityDataInfo info : nearList) {
                    //获取附近城市数据包大小总和
                    sum =  sum.add(info.getDownLoadInfo().getFullZipSize());
                    //获取附近城市未下载数量
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY) {
                        downloadedCount = downloadedCount + 1;
                    }
                }
                mBinding.tvNearDataCount.setText(count + "个城市  共" + StringUtils.formatSize(sum));
                if (downloadedCount == count) {
                    mViewModel.mNearDownloadBtnVisibility.setValue(true);
                } else {
                    mViewModel.mNearDownloadBtnVisibility.setValue(false);
                }
            }
        });
    }

    /**
     * 刷新附近城市推荐 - 全部下载按显隐状态
     */
    public void notifyNearDataView() {
        ThreadManager.getInstance().postUi(() -> {
            final ArrayList<CityDataInfo> nearList = mViewModel.getNearCityData();
            if (nearList != null && !nearList.isEmpty()) {
                int downloadedCount = 0;
                for (CityDataInfo info : nearList) {
                    //获取附近城市未下载数量
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY) {
                        downloadedCount = downloadedCount + 1;
                    }
                }

                Logger.d(TAG, "notifyNearDataView downloadedCount = " + downloadedCount);

                if (downloadedCount == nearList.size()) {
                    mViewModel.mNearDownloadBtnVisibility.setValue(true);
                } else {
                    mViewModel.mNearDownloadBtnVisibility.setValue(false);
                }
            }
        });
    }

    /**
     * 是否首次下载基础功能包提示
     */
    public void showCountryMapDataDialog() {
        mDownloadCountryDialog = new DownloadCountryDialog.Build(getContext())
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                       // 下载基础包
                        ThreadManager.getInstance().postDelay(() -> {
                            final ArrayList<Integer> adCodeList = new ArrayList<>();
                            adCodeList.add(0);
                            if (mViewModel != null) {
                                mViewModel.startAllTask(adCodeList);
                            }
                        }, 0);
                    }

                    @Override
                    public void onCancelClick() {

                    }
                }).build();
        clearBackground(mDownloadCountryDialog.getWindow());
        mDownloadCountryDialog.show();
    }

    /**
     * 清除弹窗背景
     * @param window
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }
}
