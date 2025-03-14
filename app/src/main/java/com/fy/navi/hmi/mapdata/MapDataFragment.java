package com.fy.navi.hmi.mapdata;

import android.annotation.SuppressLint;
import android.view.View;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.MapDataAdapter;
import com.fy.navi.hmi.mapdata.adapter.MuliteRecycleAdapter;
import com.fy.navi.hmi.mapdata.adapter.WorkedQueueAdapter;
import com.fy.navi.hmi.mapdata.adapter.WorkingQueueAdapter;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * @Description 离线数据页面
 * @Author fh
 * @date 2024/12/09
 */
public class MapDataFragment extends BaseFragment<FragmentMapDataBinding, MapDataViewModel> {
    private static final String TAG = MapDataFragment.class.getName();
    private MapDataAdapter mapDataAdapter;
    private List<MuliteRecycleAdapter.DataTree<String, String>> dataList = new ArrayList<>();
    private WorkingQueueAdapter workingQueueAdapter;
    private WorkedQueueAdapter workedQueueAdapter;

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
        // 初始化下载管理列表
//        initWorkingQueueView();
    }

    @Override
    public void onInitData() {
        if (mViewModel != null) {
            mViewModel.getAllProvinceData();
        }
    }

    private void initMapDataView() {
        mapDataAdapter = new MapDataAdapter(getActivity());
        mBinding.rvOffline.setLayoutManager(new LinearLayoutManager(getActivity()));
        mBinding.rvOffline.setItemAnimator(null);
        mapDataAdapter.setData(dataList);
        mBinding.rvOffline.setAdapter(mapDataAdapter);

        //以下是对布局进行控制，让省份占一行，城市占两列，效果相当于一个listView嵌套gridView的效果
        GridLayoutManager manager = new GridLayoutManager(getActivity(),1);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                return 1;
                /*return mapDataAdapter.getItemViewType(position)
                        == MuliteRecycleAdapter.ItemStatus.VIEW_TYPE_GROUP_ITEM ? 2 : 1;*/
            }
        });
        mBinding.rvOffline.setLayoutManager(manager);

        mapDataAdapter.setOfflineItemListener(new MapDataAdapter.OfflineItemListener() {
            @Override
            public void startAllTask(ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "startAllTask");
                if (mViewModel != null) {
                    mViewModel.startAllTask(cityAdCodes);
                }
            }

            @Override
            public void pauseAllTask(ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "pauseAllTask");
                if (mViewModel != null) {
                    mViewModel.pauseAllTask(cityAdCodes);
                }
            }

            @Override
            public void cancelAllTask() {
                Logger.d(TAG, "cancelAllTask");
                if (mViewModel != null) {
                    mViewModel.cancelAllTask(null);
                }
            }
        });

    }

   /* private void initWorkingQueueView() {
        //下载/更新中列表
        workingQueueAdapter = new WorkingQueueAdapter();
        workingQueueAdapter.setItemClickListener(new WorkingQueueAdapter.OnItemClickListener() {

            @Override
            public void onItemSuspendClick(int index) {

            }

        });
        LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadingOffline.setLayoutManager(layoutManager);
        mBinding.rvDownloadingOffline.setAdapter(workingQueueAdapter);

        //已下载列表
        workedQueueAdapter = new WorkedQueueAdapter();
        workedQueueAdapter.setItemClickListener(new WorkedQueueAdapter.OnItemClickListener() {

            @Override
            public void onItemDeleteClick(ArrayList<Integer> cityAdCodes) {
                Logger.d(TAG, "deleteAllTask");
                // 删除对应的已下载数据
                if (mViewModel != null) {
                    mViewModel.deleteAllTask(1, cityAdCodes);
                }
            }

        });
        LinearLayoutManager layoutManager1 = new LinearLayoutManager(getActivity());
        layoutManager1.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvDownloadedOffline.setLayoutManager(layoutManager1);
        mBinding.rvDownloadedOffline.setAdapter(workedQueueAdapter);
    }*/

    // 显示全部省份+城市信息
    public void updateMapDataView(List<ProvDataInfo> provinceBeans) {
        ThreadManager.getInstance().postUi(() -> {
            dataList.clear();
            if (provinceBeans != null && !provinceBeans.isEmpty()) {
                for (int i = 0; i < provinceBeans.size(); i++) {
                    List<CityDataInfo> city = provinceBeans.get(i).cityInfoList;
                    dataList.add(new MuliteRecycleAdapter.DataTree<>(provinceBeans.get(i).name, city));
                }
                mapDataAdapter.notifyNewData(dataList);
            }
        });
    }

    //更新数据列表下载进度&状态
    public void notifyMapDataChangeView(ProvDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            CityDataInfo cityDataInfo = info.cityInfoList.get(0);
            Logger.d(TAG, "notifyMapDataChangeView  cityDataInfo = " + GsonUtils.toJson(cityDataInfo));
            for (int i = 0; i < mapDataAdapter.getData().size(); i++) {
                for (int j = 0; j < mapDataAdapter.getSubItem(i).size(); j++) {
                    if (mapDataAdapter.getSubItem(i).get(j).adcode == cityDataInfo.adcode) {
                        Logger.d(TAG, "notifyMapDataPercent  cityDataInfo.adcode = " + cityDataInfo.adcode);
                        mapDataAdapter.getSubItem(i).set(j, cityDataInfo);
                        mapDataAdapter.notifyDataSetChanged();
                        break;
                    }
                }
            }
        });
    }

    // 显示当前城市信息
    public void updateCurrentCityView(CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            if (info != null && info.downLoadInfo != null) {
                String sizeString = StringUtils.formatSize(info.downLoadInfo.nFullZipSize);
                mBinding.currentCityData.setText(info.name + "   " + sizeString);
                mBinding.currentCityStatusTip.setText(info.downLoadInfo.statusTip);

                //已下载状态显示
                if (info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                    mBinding.downloadView.setAlpha(0.3f);
                    mBinding.downloadView.setEnabled(false);
                    mBinding.currentCityStatus.setVisibility(View.GONE);
                } else {
                    mBinding.downloadView.setAlpha(1.0f);
                    mBinding.downloadView.setEnabled(true);
                    mBinding.currentCityStatus.setVisibility(View.VISIBLE);
                }

            }
        });
    }

    // 显示基础功能包信息
    public void updateCountryDataView(CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            if (info != null && info.downLoadInfo != null) {
                mBinding.countryCityName.setText(info.name);
                String sizeString = StringUtils.formatSize(info.downLoadInfo.nFullZipSize);
                mBinding.countryDataCount.setText(sizeString);
                if (info.downLoadInfo.statusTip.equals("下载中")) {
                    mBinding.countryStatusTip.setText(info.downLoadInfo.statusTip + info.downLoadInfo.percent);
                } else {
                    mBinding.countryStatusTip.setText(info.downLoadInfo.statusTip);
                }

                //已下载状态显示
                if (info.downLoadInfo.taskState == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                    mBinding.downloadStatus.setAlpha(0.3f);
                    mBinding.downloadStatus.setEnabled(false);
                    mBinding.countryStatus.setVisibility(View.GONE);
                } else {
                    mBinding.downloadStatus.setAlpha(1.0f);
                    mBinding.downloadStatus.setEnabled(true);
                    mBinding.countryStatus.setVisibility(View.VISIBLE);
                }

            }
        });
    }

    // 显示下载中/更新中/暂停等状态信息
    public void updateWorkingView(ArrayList<CityDataInfo> downloadingList) {
        ThreadManager.getInstance().postUi(() -> {
           /* if (downloadingList != null) {
                workingQueueAdapter.setData(downloadingList);
            } else {
                // TODO: 2025/2/27
            }*/
        });
    }

    // 显示已下载信息
    public void updateWorkedView(ArrayList<CityDataInfo> downloadedList) {
        ThreadManager.getInstance().postUi(() -> {
           /* if (downloadedList != null) {
                workedQueueAdapter.setData(downloadedList);
            } else {
                // TODO: 2025/2/27
            }*/
        });
    }

    // 显示附近城市推荐信息
    @SuppressLint("SetTextI18n")
    public void updateNearDataView(ArrayList<CityDataInfo> nearList) {
        ThreadManager.getInstance().postUi(() -> {
            int count = 0;
            BigInteger sum  = BigInteger.ZERO;
            if (nearList != null && !nearList.isEmpty()) {
                count = nearList.size();
                for (CityDataInfo info : nearList) {
                    sum =  sum.add(info.downLoadInfo.nFullZipSize);
                }
            }
            String sizeString = StringUtils.formatSize(sum);
            mBinding.tvNearDataCount.setText(count + "个城市  共" + sizeString);
        });
    }

    /*public void updateMapDataTab(int type) {
        if (type == 1) { //全部地图
            mBinding.tvOfflineAllData.setTextColor(getResources().getColor(R.color.offline_black));
            mBinding.offlineAllDataLine.setVisibility(View.VISIBLE);
            mBinding.tvOfflineDownloadAdministration.setTextColor(getResources().getColor(R.color.setting_tab_gray));
            mBinding.downloadAdministrationLine.setVisibility(View.INVISIBLE);
            mBinding.viewAllData.setVisibility(View.VISIBLE);
            mBinding.viewAdministration.setVisibility(View.GONE);
        } else if (type == 2) { // 下载管理
            mBinding.tvOfflineAllData.setTextColor(getResources().getColor(R.color.setting_tab_gray));
            mBinding.offlineAllDataLine.setVisibility(View.INVISIBLE);
            mBinding.tvOfflineDownloadAdministration.setTextColor(getResources().getColor(R.color.offline_black));
            mBinding.downloadAdministrationLine.setVisibility(View.VISIBLE);
            mBinding.viewAllData.setVisibility(View.GONE);
            mBinding.viewAdministration.setVisibility(View.VISIBLE);
        }
    }*/

}
