package com.fy.navi.hmi.launcher;

import androidx.databinding.library.baseAdapters.BR;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSmallCardPlaceHolderBinding;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/27
 * Description: [小卡占位图页面]
 */
public class SmallCardPlaceHolderFragment extends BaseFragment<FragmentSmallCardPlaceHolderBinding, BaseSmallCardPlaceHolderViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_small_card_place_holder;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {

    }

    @Override
    public void onInitData() {

    }
}
