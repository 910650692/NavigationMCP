package com.fy.navi.hmi.favorite;


import android.content.Context;
import android.graphics.Rect;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.PopupWindow;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentFavoriteBinding;
import com.fy.navi.hmi.favorite.adapter.FavoriteDataAdapter;
import com.fy.navi.hmi.favorite.adapter.FrequentAddressAdapter;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinEditText;
import com.fy.navi.ui.view.SkinGridLayout;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.Objects;


public class FavoriteFragment extends BaseFragment<FragmentFavoriteBinding, FavoriteViewModel> {

    private final static String TAG = FavoriteFragment.class.getSimpleName();
    private ArrayList<PoiInfoEntity> mFavoriteList = new ArrayList<>();
    private ArrayList<PoiInfoEntity> mFrequentAddressList = new ArrayList<>();

    private FavoriteDataAdapter favoriteDataAdapter;
    private FrequentAddressAdapter frequentAddressAdapter;

    private PopupWindow popupWindow;
    private SkinTextView btnEdit;
    private SkinTextView btnDelete;
    private PopupWindow frequentPopupWindow;
    private SkinTextView renameBtn;
    private SkinGridLayout freqAddressLayout;
    private View anchorView;
    private int threeScreenHeight;
    private int scrollY;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_favorite;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initFavoriteList();
        backToTop();
//        initFrequentAddress();
    }

    @Override
    public void onInitData() {
        mViewModel.getSyncTime();
        mViewModel.getSimpleFavoriteList();
//        mViewModel.getFavoritePoiData();
        mViewModel.getHomeInfo();
        mViewModel.getCompanyInfo();
        mViewModel.initView();
        initPopupWindow();
        initFrequentPopupWindow();
        initFrequentAddressList();
    }

    @Override
    public void onResume() {
        super.onResume();
    }


    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mViewModel.getSimpleFavoriteList();
//            mViewModel.getFavoritePoiData();
            mViewModel.getHomeInfo();
            mViewModel.getCompanyInfo();
            initFrequentAddressList();
        }
    }

    /**
     * 初始化收藏点列表
     */
    private void initFavoriteList() {
        favoriteDataAdapter = new FavoriteDataAdapter();
        favoriteDataAdapter.setItemClickListener(new FavoriteDataAdapter.OnItemClickListener() {
            @Override
            public void onItemNaviClick(int index) {
                PoiInfoEntity poiInfoEntity = mFavoriteList.get(index);
                Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
                addFragment(new RouteFragment(), bundle);
            }

            @Override
            public void onItemDetailClick(int index) {
                Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,  mFavoriteList.get(index));
                bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
                addFragment(new PoiDetailsFragment(), bundle);
            }

            @Override
            public void onItemTopClick(int index) {
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    mViewModel.topFavorite(mFavoriteList.get(index), true);
                    long current = System.currentTimeMillis();
                    mViewModel.updateFavoriteTopTime(mFavoriteList.get(index).getFavoriteInfo().getItemId(), current);
                    mViewModel.updateFavoriteView(mViewModel.getFavoriteData(0));
                }
            }

            @Override
            public void onItemCancelTopClick(int index) {
                mBinding.favoriteScroll.setEnabled(false);
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    mViewModel.topFavorite(mFavoriteList.get(index), false);
                    mViewModel.updateFavoriteTopTime(mFavoriteList.get(index).getFavoriteInfo().getItemId(), 0);
                    mViewModel.updateFavoriteView(mViewModel.getFavoriteData(0));
                    ToastUtils.Companion.getInstance().showCustomToastView("已取消置顶");
                }
                mBinding.favoriteScroll.setEnabled(true);
            }

            @Override
            public void onItemDeleteClick(int index) {
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    mViewModel.removeFavorite(mFavoriteList.get(index));
                    mViewModel.deleteFavoriteData(mFavoriteList.get(index).getFavoriteInfo().getItemId());
                    mFavoriteList.remove(index);
                    mViewModel.updateFavoriteView(mFavoriteList);
                    ToastUtils.Companion.getInstance().showCustomToastView("已删除");
                }

            }
        });
        LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvFavoriteList.setNestedScrollingEnabled(false);
        mBinding.rvFavoriteList.setLayoutManager(layoutManager);
        mBinding.rvFavoriteList.setAdapter(favoriteDataAdapter);
    }

    /**
     * 更新家的信息
     * @param home 家
     */
    public void updateHomeView(PoiInfoEntity home) {
        if (home != null) {
            mBinding.favoriteHomeName.setText(home.getName());
        } else {
            mBinding.favoriteHomeName.setText(R.string.favorite_setting);
        }
    }

    /**
     * 更新公司的信息
     * @param company 公司
     */
    public void updateCompanyView(PoiInfoEntity company) {
        if (company != null) {
            mBinding.favoriteCompanyName.setText(company.getName());
        } else {
            mBinding.favoriteCompanyName.setText(R.string.favorite_setting);
        }
    }

    public View getHomeOrCompanyEditView(boolean isHome) {
        if (isHome) {
            return mBinding.moreHome;
        } else {
            return mBinding.moreCompany;
        }
    }

    public void updateSyncTime(String syncTime) {
        mBinding.favoriteTime.setText(syncTime);
    }

    private int index;
    private void initFrequentAddressList() {
        freqAddressLayout = mBinding.frequentAddressContainer;
        freqAddressLayout.removeAllViews();

        mFrequentAddressList = mViewModel.getFavoriteData(3);

        int addressCount = mFrequentAddressList.size();
        int maxItemPerRow = 2;

        // 添加地址项
        for (int i = 0; i < addressCount; i++) {
            View itemView = LayoutInflater.from(getContext()).inflate(R.layout.item_frequent_address, freqAddressLayout, false);
            SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
            SkinImageView btnMore = itemView.findViewById(R.id.tv_frequent_address_more);
            tvName.setText(mFrequentAddressList.get(i).getName());
            boolean isRename = mFrequentAddressList.get(i).getFavoriteInfo().getCustom_name() != null && !mFrequentAddressList.get(i).getFavoriteInfo().getCustom_name().equals(mFrequentAddressList.get(i).getName());
            if (isRename) {
                String name = mFrequentAddressList.get(i).getFavoriteInfo().getCustom_name();
                tvName.setText(name);
            }
            int finalI = i;
            btnMore.setOnClickListener(v -> {
                this.anchorView = itemView;
                index = finalI;
                showFrequentPopupWindow(itemView);
            });
            tvName.setOnClickListener(v -> {
                PoiInfoEntity poiInfoEntity = mFrequentAddressList.get(index);
                Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
                addFragment(new RouteFragment(), bundle);
            });

            SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(i / maxItemPerRow, 1);
            SkinGridLayout.Spec colSpec = SkinGridLayout.spec(i % maxItemPerRow, 1);

            SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_417);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.bottomMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            params.rightMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            freqAddressLayout.addView(itemView, params);
        }

        // 添加按钮逻辑
        if (addressCount <= 3) {
            View addButton = LayoutInflater.from(getContext()).inflate(R.layout.item_add_frequent_address, freqAddressLayout, false);
            int targetRow = addressCount / maxItemPerRow; // 计算应插入的行
            int targetCol = addressCount % maxItemPerRow; // 计算应插入的列

            addButton.setOnClickListener(v -> {
                if (mFrequentAddressList.size() >= 3) {
                    ToastUtils.Companion.getInstance().showCustomToastView("最多添加3个常用地址");
                } else {
                    Bundle bundle = new Bundle();
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMMON);
                    addFragment(new HomeCompanyFragment(), bundle);
                }
            });

            SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(targetRow, 1);
            SkinGridLayout.Spec colSpec = SkinGridLayout.spec(targetCol, 1);

            SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.rightMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            freqAddressLayout.addView(addButton, params);
        }
    }


    private void showRenameDialog(PoiInfoEntity address, View itemView) {
        SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
        SkinEditText etName = itemView.findViewById(R.id.tv_frequent_address_title);

        // 切换视图状态
        tvName.setVisibility(View.GONE);
        etName.setVisibility(View.VISIBLE);
        etName.setText(address.getName());

        // 焦点与键盘控制
        etName.post(() -> {
            etName.requestFocus();
            etName.setSelection(etName.getText().length()); // 光标置于末尾
            InputMethodManager imm = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
            imm.showSoftInput(etName, InputMethodManager.SHOW_IMPLICIT);
        });

        // 完成编辑监听
        etName.setOnEditorActionListener((v, actionId, event) -> {
            if (actionId == EditorInfo.IME_ACTION_DONE) {
                saveRenameResult(address, tvName, etName);
                return true;
            }
            return false;
        });

        // 失去焦点时自动保存
        etName.setOnFocusChangeListener((v, hasFocus) -> {
            if (!hasFocus) {
                saveRenameResult(address, tvName, etName);
            }
        });
    }

    private void saveRenameResult(PoiInfoEntity address, SkinTextView tvName, SkinEditText etName) {
        String newName = etName.getText().toString().trim();
        mViewModel.modifyFavoriteData(address.getFavoriteInfo().getItemId(), newName);
        if (!newName.isEmpty()) {
            address.setName(newName);
            tvName.setText(newName);
        }

        // 恢复视图状态
        tvName.setVisibility(View.VISIBLE);
        etName.setVisibility(View.GONE);

        // 隐藏键盘
        InputMethodManager imm = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(etName.getWindowToken(), 0);
    }



    /**
     * 更新常去地址
     * @param list 常去地址
     */
    public void updateFavoritePoiData(ArrayList<PoiInfoEntity> list) {
        Logger.i(TAG, "updateFavoritePoiData -> " + GsonUtils.toJson(list));
        mFrequentAddressList = list;
        ThreadManager.getInstance().postUi(() -> {
            frequentAddressAdapter.setData(mFrequentAddressList);
        });
    }

    /**
     * 更新收藏点列表
     * @param list 普通收藏点
     */
    public void updateFavoriteView(ArrayList<PoiInfoEntity> list) {
        mFavoriteList = list;

        Logger.i(TAG, "setFavoriteData -> " + GsonUtils.toJson(mFavoriteList));

        ThreadManager.getInstance().postUi(() -> {
            favoriteDataAdapter.setData(mFavoriteList);

            ViewGroup.LayoutParams params = mBinding.rvFavoriteList.getLayoutParams();
            params.height = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_150) * mFavoriteList.size();
            mBinding.rvFavoriteList.setLayoutParams(params);
        });
    }

    /**
     * 初始化常去地址列表
     */
    private void initFrequentAddress() {
        frequentAddressAdapter = new FrequentAddressAdapter(getActivity());
        frequentAddressAdapter.setItemClickListener(new FrequentAddressAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(int index) {

            }

            @Override
            public void onItemDeleteClick(int index) {


            }

            @Override
            public void onItemAddClick(int index) {

            }
        });
        // 创建布局管理器，指定每行的列数
        GridLayoutManager layoutManager = new GridLayoutManager(getActivity(), 2); // 例如，每行4列
        // 添加间距装饰
        int spacingInPixels = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
        mBinding.rvFrequentAddressList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
                outRect.bottom = spacingInPixels; // 设置底部间距
            }
        });
        mBinding.rvFrequentAddressList.setNestedScrollingEnabled(false);
        mBinding.rvFrequentAddressList.setLayoutManager(layoutManager);
        mBinding.rvFrequentAddressList.setAdapter(frequentAddressAdapter);
    }

    private void initPopupWindow() {

        // 创建PopupWindow的视图
        View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        btnEdit = popupView.findViewById(R.id.favorite_item_rename);
        btnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        popupWindow = new PopupWindow(popupView, 300, 120,true);
        popupWindow.setContentView(popupView);
        popupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        btnEdit.setOnClickListener(v -> {
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            if (mViewModel.getIsHome()) {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            } else {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            }
            addFragment(new HomeCompanyFragment(), bundle);
            popupWindow.dismiss();
        });

        btnDelete.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            boolean isHome = mViewModel.getIsHome();
            mViewModel.removeFavorite(mViewModel.getHomeCompanyInfo(isHome));
            mViewModel.deleteFavoriteData(mViewModel.getHomeCompanyInfo(isHome).getFavoriteInfo().getItemId());
            if (isHome) {
                mViewModel.updateHomeView(null);
            } else {
                mViewModel.updateCompanyView(null);
            }
            popupWindow.dismiss();
        });
    }

    public void showPopupWindow(View anchorView) {
        // 显示PopupWindow在更多按钮的上方
        int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        int x = location[0] - 220;
        int y = location[1] - popupWindow.getHeight(); // 计算PopupWindow的Y坐标

        btnEdit.setTextColor(getResources().getColor(R.color.black));
        btnEdit.setText(R.string.favorite_item_edit);
        if (((mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(true) == null)) ||
                (!mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(false) == null)) {
            btnDelete.setEnabled(false);
            btnDelete.setTextColor(getResources().getColor(R.color.setting_tab_gray));
        } else {
            btnDelete.setEnabled(true);
            btnDelete.setTextColor(getResources().getColor(R.color.black));
        }
        popupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }

    private void initFrequentPopupWindow() {

        // 创建PopupWindow的视图
        View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        renameBtn = popupView.findViewById(R.id.favorite_item_rename);
        btnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        frequentPopupWindow = new PopupWindow(popupView, 300, 120, true);
        frequentPopupWindow.setContentView(popupView);
        frequentPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        renameBtn.setOnClickListener(v -> {
            showRenameDialog(mFrequentAddressList.get(index), anchorView);
            frequentPopupWindow.dismiss();
        });

        btnDelete.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.removeFavorite(mFrequentAddressList.get(index));
                mViewModel.deleteFavoriteData(mFrequentAddressList.get(index).getFavoriteInfo().getItemId());
                initFrequentAddressList();
            });
            frequentPopupWindow.dismiss();
        });
    }

    public void showFrequentPopupWindow(View anchorView) {
        int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        int x = location[0] + 320;
        int y = location[1] - frequentPopupWindow.getHeight(); // 计算PopupWindow的Y坐标
        frequentPopupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }

   private void backToTop() {

       threeScreenHeight = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_714)  * 3;

       // 设置滚动监听
       mBinding.favoriteScroll.getViewTreeObserver().addOnScrollChangedListener(
               () -> {
                   int scrollY = mBinding.favoriteScroll.getScrollY();
                   Logger.d("scrollY", "scrollY: " + scrollY);
                   mBinding.layoutTop.setVisibility(
                           scrollY >= threeScreenHeight ? View.VISIBLE : View.INVISIBLE
                   );
               }
       );

       // 按钮点击事件
       mBinding.layoutTop.setOnClickListener(v -> {
           mBinding.favoriteScroll.smoothScrollTo(0, 0); // 平滑滚动
       });
   }
}
