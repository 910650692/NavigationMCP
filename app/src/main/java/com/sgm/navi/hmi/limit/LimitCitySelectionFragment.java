package com.sgm.navi.hmi.limit;

import android.content.Context;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.inputmethod.InputMethodManager;
import android.widget.TextView;

import androidx.databinding.library.baseAdapters.BR;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentLimitCitySelectionBinding;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

/**
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/20
 * Description: [城市选择界面]
 */
public class LimitCitySelectionFragment extends BaseFragment<FragmentLimitCitySelectionBinding, LimitDriverViewModel> {
    private LimitProvincesAdapter mAdapter;
    private ArrayList<ProvDataInfo> mProvDataInfos;
    private LimitCitiesAdapter mSearchCitiesAdapter;

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
        mSearchCitiesAdapter = new LimitCitiesAdapter(requireContext(), new ArrayList<>());
        mBinding.searchRecyclerView.setLayoutManager(new GridLayoutManager(requireContext(), 3));
        mBinding.searchRecyclerView.setAdapter(mSearchCitiesAdapter);
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

        mSearchCitiesAdapter.setListener(new LimitCitiesAdapter.ItemClickListener() {
            @Override
            public void onClick(String cityCode) {
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
                if (charSequence.length() > 0) {
                    mBinding.editTextId.setHint(mBinding.editTextId.getHint().toString());
                } else {
                    if (mBinding.editTextId.getHint().toString().length() > 10) {
                        mBinding.editTextId.setHint(mBinding.editTextId.getHint().toString().substring(0, 10) + "....");
                    } else {
                        mBinding.editTextId.setHint(mBinding.editTextId.getHint().toString());
                    }
                }
            }

            @Override
            public void afterTextChanged(final Editable editable) {
                final String editText = editable.toString();
                if(mProvDataInfos == null) {
                    return;
                }
                if (editText.isEmpty()) {
                    mViewModel.mTextViewVisibility.setValue(false);
                    mViewModel.mCloseViewVisibility.setValue(false);
                    mViewModel.mSearchCityVisibility.setValue(false);
                    return;
                } else {
                    mViewModel.mCloseViewVisibility.setValue(true);
                }

                List<CityDataInfo> mSearchDate = new ArrayList<>();
                for (int i = 0; i < mProvDataInfos.size(); i++) {
                    for (CityDataInfo cityDataInfo :mProvDataInfos.get(i).getCityInfoList()) {
                        if (cityDataInfo.getName() == null) {
                            continue;
                        }
                        if (cityDataInfo.getName().contains(editText)) {
                            mSearchDate.add(cityDataInfo);
                        }
                    }
                }
                if (mSearchDate.isEmpty()) {
                    mViewModel.mTextViewContent.setValue("没有找到\"" + editText + "\"相关的城市");
                    mViewModel.mTextViewVisibility.setValue(true);
                    mViewModel.mSearchCityVisibility.setValue(false);
                } else {
                    mViewModel.mTextViewVisibility.setValue(false);
                    mSearchCitiesAdapter.setData(mSearchDate);
                    mViewModel.mSearchCityVisibility.setValue(true);

                }
            }
        });

        mViewModel.mEditTextContent.observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                mBinding.editTextId.setText(s);
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
