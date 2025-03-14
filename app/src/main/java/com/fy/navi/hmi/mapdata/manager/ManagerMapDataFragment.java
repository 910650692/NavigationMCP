package com.fy.navi.hmi.mapdata.manager;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentManagerMapDataBinding;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Description 下载管理页面
 * @Author fh
 * @date 2025/03/13
 */
public class ManagerMapDataFragment extends BaseFragment<FragmentManagerMapDataBinding, ManagerMapDataViewModel> {

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

    }

    @Override
    public void onInitData() {

    }



}
