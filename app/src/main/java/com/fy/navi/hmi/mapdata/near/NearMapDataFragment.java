package com.fy.navi.hmi.mapdata.near;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentNearMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.CityMapDataAdapter;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

/**
 * @Description 附近城市推荐页面
 * @Author fh
 * @date 2025/03/13
 */
public class NearMapDataFragment extends BaseFragment<FragmentNearMapDataBinding, NearMapDataViewModel> {
    private CityMapDataAdapter cityMapDataAdapter;

    private ArrayList<CityDataInfo> nearCityDataInfos;
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

    private void initNearMapDataView() {
        cityMapDataAdapter = new CityMapDataAdapter(getActivity());
        LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvNearOffline.setLayoutManager(layoutManager);
        mBinding.rvNearOffline.setAdapter(cityMapDataAdapter);
        cityMapDataAdapter.setItemClickListener(new CityMapDataAdapter.OnItemClickListener() {

            @Override
            public void onItemClick(ArrayList<Integer> cityAdCodes) {
                Logger.d( "onItemClick cityAdCodes = " + GsonUtils.toJson(cityAdCodes));
                mViewModel.startAllTask(cityAdCodes);
            }

        });

    }

    /**
     * 更新数据状态
     */
    public void updateNearView(ArrayList<CityDataInfo> cityDataInfos) {

        nearCityDataInfos = cityDataInfos;

        ThreadManager.getInstance().postUi(() -> {
            if (cityDataInfos != null && !cityDataInfos.isEmpty()) {
                cityMapDataAdapter.setData(cityDataInfos);
            }
        });
    }

}
