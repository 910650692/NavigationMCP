package com.fy.navi.hmi.limit;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;

import androidx.databinding.library.baseAdapters.BR;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentLimitCitySelectionBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

/**
 * Author: LiuChang
 * Date: 2025/2/20
 * Description: [城市选择界面]
 */
public class LimitCitySelectionFragment extends BaseFragment<FragmentLimitCitySelectionBinding, LimitDriverViewModel> {
    private LimitProvincesAdapter adapter;
    private ArrayList<ProvDataInfo> mProvDataInfos;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_limit_city_selection;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        adapter = new LimitProvincesAdapter(requireContext(), new ArrayList<>());
        mBinding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.recyclerView.setAdapter(adapter);
    }

    @Override
    public void onInitData() {
        Bundle bundle = getArguments();
        String cityName = (String) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_SELECTION);
        if (cityName != null) {
            mBinding.selectCityName.setText(cityName);
        }
        mProvDataInfos = MapDataPackage.getInstance().getMapDataList();
        adapter.setData(mProvDataInfos);
        onInitClick();
    }

    public void onInitClick() {
        adapter.setListener(new LimitCitiesAdapter.ItemClickListener() {
            @Override
            public void onClick(String cityCode) {
                closeAllFragmentsUntilTargetFragment(LimitDriveFragment.class.getName());
                Bundle bundle = new Bundle();
                bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_TASK_ID, cityCode);
                addFragment(new LimitDriveFragment(), bundle);
                closeAllFragmentsUntilTargetFragment(LimitCitySelectionFragment.class.getName());
            }
        });

        mBinding.editTextId.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence charSequence, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                String editText = editable.toString();
                if(mProvDataInfos == null) {
                    return;
                }
                for (int i = 0; i < mProvDataInfos.size(); i++) {
                    if (mProvDataInfos.get(i).name.contains(editText)) {
                        LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding
                                .recyclerView.getLayoutManager();
                        if (layoutManager != null) {
                            layoutManager.scrollToPositionWithOffset(i ,0);
                        }
                        break;
                    }
                    for (CityDataInfo cityDataInfo :mProvDataInfos.get(i).cityInfoList) {
                        if (cityDataInfo.name.contains(editText)) {
                            LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding
                                    .recyclerView.getLayoutManager();
                            if (layoutManager != null) {
                                layoutManager.scrollToPositionWithOffset(i ,0);
                            }
                            break;
                        }
                    }
                }
            }
        });
    }

}
