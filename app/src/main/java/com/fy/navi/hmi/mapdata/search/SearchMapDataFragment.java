package com.fy.navi.hmi.mapdata.search;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.TextView;

import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSearchMapDataBinding;
import com.fy.navi.hmi.mapdata.adapter.BaseSearchMapDataAdapter;
import com.fy.navi.hmi.mapdata.adapter.SearchMapDataAdapter;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description 离线数据搜索结果列表页面
 * @Author fh
 * @date 2024/02/17
 */
public class SearchMapDataFragment extends BaseFragment<FragmentSearchMapDataBinding, SearchMapDataViewModel> {
    private SearchMapDataAdapter searchMapDataAdapter;
    private List<BaseSearchMapDataAdapter.DataTree<ProvDataInfo, String>> dataList = new ArrayList<>();

    @Override
    public int onLayoutId() {
        return R.layout.fragment_search_map_data;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initSearchMapDataView();
        setupSearchActions();
        requestFocusAndShowKeyboard();
    }

    @Override
    public void onInitData() {

    }

    private void initSearchMapDataView() {
        searchMapDataAdapter = new SearchMapDataAdapter(getActivity());
        mBinding.rvSearchOffline.setLayoutManager(new LinearLayoutManager(getActivity()));
        mBinding.rvSearchOffline.setItemAnimator(null);
        searchMapDataAdapter.setData(dataList);
        mBinding.rvSearchOffline.setAdapter(searchMapDataAdapter);

        //以下是对布局进行控制，让省份占一行，城市占两列，效果相当于一个listView嵌套gridView的效果
        GridLayoutManager manager = new GridLayoutManager(getActivity(),1);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                return 1;
            }
        });
        mBinding.rvSearchOffline.setLayoutManager(manager);

        searchMapDataAdapter.setOfflineItemListener(new SearchMapDataAdapter.OfflineItemListener() {
            @Override
            public void startAllTask(int downloadMode) {
                if (mViewModel != null) {
                    mViewModel.startAllTask(null);
                }
            }

            @Override
            public void pauseAllTask(int downloadMode) {
                if (mViewModel != null) {
                    mViewModel.pauseAllTask(null);
                }
            }

            @Override
            public void cancelAllTask(int downloadMode) {
                if (mViewModel != null) {
                    mViewModel.cancelAllTask(null);
                }
            }
        });

    }

    /**
     * 搜索相关事件
     */
    private void setupSearchActions() {
        mBinding.searchOfflineEditView.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable editable) {
                if (!editable.toString().trim().isEmpty()) {
                    mBinding.editClear.setVisibility(View.VISIBLE);
                    mViewModel.searchAdCode(editable.toString().trim());
                } else {
                    mBinding.editClear.setVisibility(View.GONE);
                }
            }
        });

        mBinding.searchOfflineEditView.setOnEditorActionListener(new TextView.OnEditorActionListener() {
            @Override
            public boolean onEditorAction(TextView textView, int actionId, KeyEvent event) {
                if (actionId == EditorInfo.IME_ACTION_DONE ||
                        (event != null && event.getKeyCode() == KeyEvent.KEYCODE_ENTER && event.getAction() == KeyEvent.ACTION_DOWN)) {
                    hideInput();
                    return false;
                } else {
                    return false;
                }
            }
        });

    }

    /**
     * 请求焦点并显示软键盘
     */
    public void requestFocusAndShowKeyboard() {
        // 确保视图已经附加到窗口
        mBinding.searchOfflineEditView.post(() -> {
            mBinding.searchOfflineEditView.requestFocus();
            InputMethodManager imm = (InputMethodManager) mActivity.getSystemService(Context.INPUT_METHOD_SERVICE);
            if (imm != null) {
                // 显示软键盘
                imm.showSoftInput(mBinding.searchOfflineEditView, InputMethodManager.SHOW_IMPLICIT);
            }
        });
    }

    /**
     * 隐藏软键盘
     */
    public void hideInput() {
       /* InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(getWindowToken(), 0);
        }*/

        InputMethodManager imm = (InputMethodManager) mActivity.getSystemService(Context.INPUT_METHOD_SERVICE);
        if (null != imm) {
            imm.hideSoftInputFromWindow(mActivity.getWindow().getDecorView().getWindowToken(), 0);
        }
    }

    /**
     * 更新搜索结果
     */
    public void updateSearchResultView(ArrayList<ProvDataInfo> provinceBeans) {
        ThreadManager.getInstance().postUi(() -> {
            dataList.clear();
            if (provinceBeans != null && !provinceBeans.isEmpty()) {

                mBinding.noSearchOfflineView.setVisibility(View.GONE);
                mBinding.rvSearchOffline.setVisibility(View.VISIBLE);

                for (int i = 0; i < provinceBeans.size(); i++) {
                    List<CityDataInfo> city = provinceBeans.get(i).cityInfoList;
                    dataList.add(new BaseSearchMapDataAdapter.DataTree<>(provinceBeans.get(i), city));
                }
                searchMapDataAdapter.notifyNewData(dataList);
            } else {

                mBinding.noSearchOfflineView.setVisibility(View.VISIBLE);
                mBinding.rvSearchOffline.setVisibility(View.GONE);

            }
        });
    }

    /**
     * 清空输入框内容
     */
    public void clearEditText() {
        mBinding.searchOfflineEditView.setText("");
        if (null != searchMapDataAdapter) {
            dataList.clear();
            searchMapDataAdapter.notifyNewData(dataList);
        }
    }

}
