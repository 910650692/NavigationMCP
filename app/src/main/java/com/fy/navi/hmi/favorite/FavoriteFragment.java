package com.fy.navi.hmi.favorite;


import android.content.Context;
import android.graphics.Rect;
import android.os.Bundle;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
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

    private FavoriteDataAdapter mFavoriteDataAdapter;
    private FrequentAddressAdapter mFrequentAddressAdapter;

    private PopupWindow mPopupWindow;
    private SkinTextView mBtnEdit;
    private SkinTextView mBtnDelete;
    private PopupWindow mFrequentPopupWindow;
    private SkinTextView mRenameBtn;
    private SkinGridLayout mFreqAddressLayout;
    private View mAnchorView;
    private int mThreeScreenHeight;

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
    public void onHiddenChanged(final boolean hidden) {
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
        mFavoriteDataAdapter = new FavoriteDataAdapter();
        mFavoriteDataAdapter.setItemClickListener(new FavoriteDataAdapter.OnItemClickListener() {
            @Override
            public void onItemNaviClick(final int index) {
                final PoiInfoEntity poiInfoEntity = mFavoriteList.get(index);
                final Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
                addFragment(new RouteFragment(), bundle);
            }

            @Override
            public void onItemDetailClick(final int index) {
                final Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL,  mFavoriteList.get(index));
                bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
                addPoiDetailsFragment(new PoiDetailsFragment(), bundle);
            }

            @Override
            public void onItemTopClick(final int index) {
                if (mFavoriteList != null && !mFavoriteList.isEmpty()) {
                    SettingUpdateObservable.getInstance().onUpdateSyncTime();
                    mViewModel.topFavorite(mFavoriteList.get(index), true);
                    final long current = System.currentTimeMillis();
                    mViewModel.updateFavoriteTopTime(mFavoriteList.get(index).getFavoriteInfo().getItemId(), current);
                    mViewModel.updateFavoriteView(mViewModel.getFavoriteData(0));
                }
            }

            @Override
            public void onItemCancelTopClick(final int index) {
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
            public void onItemDeleteClick(final int index) {
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
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.rvFavoriteList.setNestedScrollingEnabled(false);
        mBinding.rvFavoriteList.setLayoutManager(layoutManager);
        mBinding.rvFavoriteList.setAdapter(mFavoriteDataAdapter);
    }

    /**
     * 更新家的信息
     * @param home 家
     */
    public void updateHomeView(final PoiInfoEntity home) {
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
    public void updateCompanyView(final PoiInfoEntity company) {
        if (company != null) {
            mBinding.favoriteCompanyName.setText(company.getName());
        } else {
            mBinding.favoriteCompanyName.setText(R.string.favorite_setting);
        }
    }

    /**
     * getHomeOrCompanyEditView
     * @param isHome
     * @return view
     */
    public View getHomeOrCompanyEditView(final boolean isHome) {
        if (isHome) {
            return mBinding.moreHome;
        } else {
            return mBinding.moreCompany;
        }
    }

    /**
     * updateSyncTime
     * @param syncTime
     */
    public void updateSyncTime(final String syncTime) {
        mBinding.favoriteTime.setText(syncTime);
    }

    private int mIndex;

    /**
     * initFrequentAddressList
     */
    private void initFrequentAddressList() {
        mFreqAddressLayout = mBinding.frequentAddressContainer;
        mFreqAddressLayout.removeAllViews();

        mFrequentAddressList = mViewModel.getFavoriteData(3);

        final int addressCount = mFrequentAddressList.size();
        final int maxItemPerRow = 2;

        // 添加地址项
        for (int i = 0; i < addressCount; i++) {
            final View itemView = LayoutInflater.from(getContext()).inflate(R.layout.item_frequent_address, mFreqAddressLayout, false);
            final SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
            final SkinImageView btnMore = itemView.findViewById(R.id.tv_frequent_address_more);
            tvName.setText(mFrequentAddressList.get(i).getName());
            final String name = mFrequentAddressList.get(i).getFavoriteInfo().getCustom_name();
            boolean isRename = false;
            if (name != null) {
                isRename = !Objects.equals(name, mFrequentAddressList.get(i).getName());
            }
            if (isRename) {
                tvName.setText(name);
            }
            final int finalI = i;
            btnMore.setOnClickListener(v -> {
                this.mAnchorView = itemView;
                mIndex = finalI;
                showFrequentPopupWindow(itemView);
            });
            tvName.setOnClickListener(v -> {
                final PoiInfoEntity poiInfoEntity = mFrequentAddressList.get(mIndex);
                final Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
                addFragment(new RouteFragment(), bundle);
            });

            final SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(i / maxItemPerRow, 1);
            final SkinGridLayout.Spec colSpec = SkinGridLayout.spec(i % maxItemPerRow, 1);

            final SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_417);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.bottomMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            params.rightMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            mFreqAddressLayout.addView(itemView, params);
        }

        // 添加按钮逻辑
        if (addressCount <= 3) {
            final View addButton = LayoutInflater.from(getContext()).inflate(R.layout.item_add_frequent_address, mFreqAddressLayout, false);
            final int targetRow = addressCount / maxItemPerRow; // 计算应插入的行
            final int targetCol = addressCount % maxItemPerRow; // 计算应插入的列

            addButton.setOnClickListener(v -> {
                if (mFrequentAddressList.size() >= 3) {
                    ToastUtils.Companion.getInstance().showCustomToastView("最多添加3个常用地址");
                } else {
                    final Bundle bundle = new Bundle();
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
                    bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMMON);
                    addFragment(new HomeCompanyFragment(), bundle);
                }
            });

            final SkinGridLayout.Spec rowSpec = SkinGridLayout.spec(targetRow, 1);
            final SkinGridLayout.Spec colSpec = SkinGridLayout.spec(targetCol, 1);

            final SkinGridLayout.LayoutParams params = new SkinGridLayout.LayoutParams(rowSpec, colSpec);
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_130);
            params.rightMargin = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
            mFreqAddressLayout.addView(addButton, params);
        }
    }


    /**
     * showRenameDialog
     * @param address
     * @param itemView
     */
    private void showRenameDialog(final PoiInfoEntity address, final View itemView) {
        final SkinTextView tvName = itemView.findViewById(R.id.tv_frequent_address_text);
        final SkinEditText etName = itemView.findViewById(R.id.tv_frequent_address_title);

        // 切换视图状态
        tvName.setVisibility(View.GONE);
        etName.setVisibility(View.VISIBLE);
        etName.setText(address.getName());

        // 焦点与键盘控制
        etName.post(() -> {
            etName.requestFocus();
            etName.setSelection(etName.getText().length()); // 光标置于末尾
            final InputMethodManager imm = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
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

    /**
     * saveRenameResult
     * @param address
     * @param tvName
     * @param etName
     */
    private void saveRenameResult(final PoiInfoEntity address, final SkinTextView tvName, final SkinEditText etName) {
        final String newName = etName.getText().toString().trim();
        mViewModel.modifyFavoriteData(address.getFavoriteInfo().getItemId(), newName);
        if (!newName.isEmpty()) {
            address.setName(newName);
            tvName.setText(newName);
        }

        // 恢复视图状态
        tvName.setVisibility(View.VISIBLE);
        etName.setVisibility(View.GONE);

        // 隐藏键盘
        final InputMethodManager imm = (InputMethodManager) requireContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(etName.getWindowToken(), 0);
    }



    /**
     * 更新常去地址
     * @param list 常去地址
     */
    public void updateFavoritePoiData(final ArrayList<PoiInfoEntity> list) {
        Logger.i(TAG, "updateFavoritePoiData -> " + GsonUtils.toJson(list));
        mFrequentAddressList = list;
        ThreadManager.getInstance().postUi(() -> {
            mFrequentAddressAdapter.setData(mFrequentAddressList);
        });
    }

    /**
     * 更新收藏点列表
     * @param list 普通收藏点
     */
    public void updateFavoriteView(final ArrayList<PoiInfoEntity> list) {
        mFavoriteList = list;
        Logger.i(TAG, "setFavoriteData -> " + GsonUtils.toJson(mFavoriteList));

        ThreadManager.getInstance().postUi(() -> {
            mFavoriteDataAdapter.setData(mFavoriteList);

            final ViewGroup.LayoutParams params = mBinding.rvFavoriteList.getLayoutParams();
            params.height = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_150) * mFavoriteList.size();
            mBinding.rvFavoriteList.setLayoutParams(params);
        });
    }

    /**
     * 初始化常去地址列表
     */
    private void initFrequentAddress() {
        mFrequentAddressAdapter = new FrequentAddressAdapter(getActivity());
        mFrequentAddressAdapter.setItemClickListener(new FrequentAddressAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int index) {

            }

            @Override
            public void onItemDeleteClick(final int index) {


            }

            @Override
            public void onItemAddClick(final int index) {

            }
        });
        // 创建布局管理器，指定每行的列数
        final GridLayoutManager layoutManager = new GridLayoutManager(getActivity(), 2); // 例如，每行4列
        // 添加间距装饰
        final int spacingInPixels = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_20);
        mBinding.rvFrequentAddressList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(final @NonNull Rect outRect, final @NonNull View view, final @NonNull RecyclerView parent,
                                       final @NonNull RecyclerView.State state) {
                outRect.bottom = spacingInPixels; // 设置底部间距
            }
        });
        mBinding.rvFrequentAddressList.setNestedScrollingEnabled(false);
        mBinding.rvFrequentAddressList.setLayoutManager(layoutManager);
        mBinding.rvFrequentAddressList.setAdapter(mFrequentAddressAdapter);
    }

    /**
     * initPopupWindow
     */
    private void initPopupWindow() {

        // 创建PopupWindow的视图
        final View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        mBtnEdit = popupView.findViewById(R.id.favorite_item_rename);
        mBtnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        mPopupWindow = new PopupWindow(popupView, 300, 120,true);
        mPopupWindow.setContentView(popupView);
        mPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        mBtnEdit.setOnClickListener(v -> {
            final Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            if (mViewModel.getIsHome()) {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
            } else {
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
            }
            addFragment(new HomeCompanyFragment(), bundle);
            mPopupWindow.dismiss();
        });

        mBtnDelete.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            final boolean isHome = mViewModel.getIsHome();
            mViewModel.removeFavorite(mViewModel.getHomeCompanyInfo(isHome));
            mViewModel.deleteFavoriteData(mViewModel.getHomeCompanyInfo(isHome).getFavoriteInfo().getItemId());
            if (isHome) {
                mViewModel.updateHomeView(null);
            } else {
                mViewModel.updateCompanyView(null);
            }
            mPopupWindow.dismiss();
        });
    }

    /**
     * showPopupWindow
     * @param anchorView
     */
    public void showPopupWindow(final View anchorView) {
        // 显示PopupWindow在更多按钮的上方
        final int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        final int x = location[0] - 220;
        final int y = location[1] - mPopupWindow.getHeight(); // 计算PopupWindow的Y坐标

        mBtnEdit.setTextColor(getResources().getColor(R.color.black));
        mBtnEdit.setText(R.string.favorite_item_edit);
        if (((mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(true) == null)) ||
                (!mViewModel.getIsHome() && mViewModel.getHomeCompanyInfo(false) == null)) {
            mBtnDelete.setEnabled(false);
            mBtnDelete.setTextColor(getResources().getColor(R.color.setting_tab_gray));
        } else {
            mBtnDelete.setEnabled(true);
            mBtnDelete.setTextColor(getResources().getColor(R.color.black));
        }
        mPopupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }

    /**
     * initFrequentPopupWindow
     */
    private void initFrequentPopupWindow() {

        // 创建PopupWindow的视图
        final View popupView = LayoutInflater.from(getContext()).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        mRenameBtn = popupView.findViewById(R.id.favorite_item_rename);
        mBtnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        mFrequentPopupWindow = new PopupWindow(popupView, 300, 120, true);
        mFrequentPopupWindow.setContentView(popupView);
        mFrequentPopupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        mRenameBtn.setOnClickListener(v -> {
            showRenameDialog(mFrequentAddressList.get(mIndex), mAnchorView);
            mFrequentPopupWindow.dismiss();
        });

        mBtnDelete.setOnClickListener(v -> {
            SettingUpdateObservable.getInstance().onUpdateSyncTime();
            ThreadManager.getInstance().postUi(() -> {
                mViewModel.removeFavorite(mFrequentAddressList.get(mIndex));
                mViewModel.deleteFavoriteData(mFrequentAddressList.get(mIndex).getFavoriteInfo().getItemId());
                initFrequentAddressList();
            });
            mFrequentPopupWindow.dismiss();
        });
    }

    /**
     * showFrequentPopupWindow
     * @param anchorView
     */
    public void showFrequentPopupWindow(final View anchorView) {
        final int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        final int x = location[0] + 320;
        final int y = location[1] - mFrequentPopupWindow.getHeight(); // 计算PopupWindow的Y坐标
        mFrequentPopupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }

    /**
     * backToTop
     */
   private void backToTop() {

       mThreeScreenHeight = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_714)  * 3;

       // 设置滚动监听
       mBinding.favoriteScroll.getViewTreeObserver().addOnScrollChangedListener(
               () -> {
                   final int scrollY = mBinding.favoriteScroll.getScrollY();
                   Logger.d("scrollY", "scrollY: " + scrollY);
                   mBinding.layoutTop.setVisibility(
                           scrollY >= mThreeScreenHeight ? View.VISIBLE : View.INVISIBLE
                   );
               }
       );

       // 按钮点击事件
       mBinding.layoutTop.setOnClickListener(v -> {
           mBinding.favoriteScroll.smoothScrollTo(0, 0); // 平滑滚动
       });
   }
}
