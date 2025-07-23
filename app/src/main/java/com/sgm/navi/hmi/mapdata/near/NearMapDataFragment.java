package com.sgm.navi.hmi.mapdata.near;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentNearMapDataBinding;
import com.sgm.navi.hmi.mapdata.adapter.CityMapDataAdapter;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;

public class NearMapDataFragment extends BaseFragment<FragmentNearMapDataBinding, NearMapDataViewModel> {
    private CityMapDataAdapter mCityMapDataAdapter;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_near_map_data;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initNearMapDataView();
    }

    @Override
    public void onInitData() {
        mViewModel.initData();
    }

    /**
     * 初始化附近推荐城市view
     */
    private void initNearMapDataView() {
        mCityMapDataAdapter = new CityMapDataAdapter(getActivity());
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvNearOffline.setLayoutManager(layoutManager);
        mBinding.rvNearOffline.setItemAnimator(null);
        mBinding.rvNearOffline.setAdapter(mCityMapDataAdapter);
        mCityMapDataAdapter.setItemClickListener(new CityMapDataAdapter.OnItemClickListener() {

            @Override
            public void startAllTask(final ArrayList<Integer> cityAdCodes) {
                if (Logger.openLog) {
                    Logger.d("onItemClick cityAdCodes = " + cityAdCodes);
                }
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.startAllTask(cityAdCodes);
                    }
                }, 0);
            }

            @Override
            public void pauseAllTask(final ArrayList<Integer> cityAdCodes) {
                if (Logger.openLog) {
                    Logger.d("pauseAllTask cityAdCodes = " + cityAdCodes);
                }
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.pauseAllTask(cityAdCodes);
                    }
                }, 0);
            }

            @Override
            public void deleteAllTask(final ArrayList<Integer> cityAdCodes) {
                if (Logger.openLog) {
                    Logger.d("deleteAllTask cityAdCodes = ", cityAdCodes);
                }
                ThreadManager.getInstance().postDelay(() -> {
                    if (mViewModel != null) {
                        mViewModel.deleteAllTask(cityAdCodes);
                    }
                }, 0);
            }

        });

    }

    /**
     * 设置数据状态
     * @param cityDataInfos
     */
    public void updateNearView(final ArrayList<CityDataInfo> cityDataInfos) {
        ThreadManager.getInstance().postUi(() -> {
            if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
                mCityMapDataAdapter.setData(cityDataInfos);
            }
        });
    }

    /**
     * 更新数据状态
     * @param info
     */
    public void notifyNearView(final CityDataInfo info) {
        ThreadManager.getInstance().postUi(() -> {
            mCityMapDataAdapter.updateChild(info.getAdcode(), info.getDownLoadInfo());
        });
    }

}
