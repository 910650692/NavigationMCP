package com.fy.navi.hmi.limit;

import android.content.Context;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.inputmethod.InputMethodManager;
import android.widget.TextView;

import androidx.databinding.library.baseAdapters.BR;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentLimitCitySelectionBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;

/**
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/20
 * Description: [城市选择界面]
 */
public class LimitCitySelectionFragment extends BaseFragment<FragmentLimitCitySelectionBinding, LimitDriverViewModel> {
    private LimitProvincesAdapter mAdapter;
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
        mAdapter = new LimitProvincesAdapter(requireContext(), new ArrayList<>());
        mBinding.recyclerView.setLayoutManager(new LinearLayoutManager(requireContext()));
        mBinding.recyclerView.setAdapter(mAdapter);
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        mProvDataInfos = MapDataPackage.getInstance().getMapDataList();
        mAdapter.setData(mProvDataInfos);
        onInitClick();
        if (bundle == null) {
            return;
        }
        final String cityName = (String) bundle.getSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_SELECTION);
        if (cityName != null) {
            mBinding.selectCityName.setText(cityName);
        }

    }

    /**
     * 点击事件初始化
     *
     */
    public void onInitClick() {
        mAdapter.setListener(new LimitCitiesAdapter.ItemClickListener() {
            @Override
            public void onClick(final String cityCode) {
                final Bundle bundle = new Bundle();
                bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_TASK_ID, cityCode);
                addPoiDetailsFragment(new LimitDriveFragment(), bundle);
                closeAllFragmentsUntilTargetFragment(LimitCitySelectionFragment.class.getName());
            }
        });

        mBinding.editTextId.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence charSequence, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence charSequence, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable editable) {
                final String editText = editable.toString();
                if(mProvDataInfos == null) {
                    return;
                }
                for (int i = 0; i < mProvDataInfos.size(); i++) {
                    if (mProvDataInfos.get(i).getName().contains(editText)) {
                        final LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding
                                .recyclerView.getLayoutManager();
                        if (layoutManager != null) {
                            layoutManager.scrollToPositionWithOffset(i ,0);
                        }
                        break;
                    }
                    for (CityDataInfo cityDataInfo :mProvDataInfos.get(i).getCityInfoList()) {
                        if (cityDataInfo.getName() == null) {
                            continue;
                        }
                        if (cityDataInfo.getName().contains(editText)) {
                            final LinearLayoutManager layoutManager = (LinearLayoutManager) mBinding
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

        mBinding.editTextId.setOnEditorActionListener(new TextView.OnEditorActionListener() {
            @Override
            public boolean onEditorAction(final TextView v, final int actionId, final KeyEvent event) {
                hideInput();
                return true;
            }
        });
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
        final InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(mBinding.recyclerView.getWindowToken(), 0);
        }
    }

}
