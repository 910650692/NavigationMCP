
package com.sgm.navi.scene.ui.adapter;


import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewStub;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.adapter.GasStationAdapter;
import com.sgm.navi.scene.adapter.GridSpacingItemDecoration;
import com.sgm.navi.scene.adapter.HorizontalSpaceItemDecoration;
import com.sgm.navi.scene.databinding.ScenePoiItemCateringViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiItemChargeViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiItemGasViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiItemParkingViewBinding;
import com.sgm.navi.scene.databinding.ScenePoiItemScenicSpotViewBinding;
import com.sgm.navi.scene.databinding.SearchResultItemBinding;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.ParkingInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class SearchResultAdapter extends RecyclerView.Adapter<SearchResultAdapter.ResultHolder> {
    private SearchResultEntity mSearchResultEntity;
    private final List<PoiInfoEntity> mPoiEntities;
    private OnItemClickListener mOnItemClickListener;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private PoiInfoEntity mPoiInfoEntity;
    private int mHomeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
    private boolean mIsEnd = false;
    private final int mSpanCount = 2;
    private final int mSpacing = 16;
    private final int mHorizontalSpacing = 16;
    private PoiInfoEntity mChildSelectInfo;
    private int mChildSelectIndex = -1;
    private int mParentSelectIndex;
    private int mLastParentSelectIndex;
    private boolean isFirstRefresh = true;
    private ArrayList<String> mLabelNameList;
    private List<RouteParam> mGasChargeAlongList = new ArrayList<>();
    private SearchResultFilterLabelAdapter mSearchResultFilterLabelAdapter;
    public void setOnItemClickListener(final OnItemClickListener listener) {
        mOnItemClickListener = listener;
    }

    public int getHomeCompanyType() {
        return mHomeCompanyType;
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }

    public void setIsEnd(final boolean isEnd) {
        this.mIsEnd = isEnd;
    }

    public SearchResultAdapter() {
        this.mPoiEntities = new ArrayList<>();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mLabelNameList = new ArrayList<>();
    }

    public void updateAlongList(final List<RouteParam> gasChargeAlongList) {
        mGasChargeAlongList.clear();
        mGasChargeAlongList.addAll(gasChargeAlongList);
    }

    public void updateAlongList(final List<RouteParam> gasChargeAlongList, final int index) {
        mGasChargeAlongList.clear();
        mGasChargeAlongList.addAll(gasChargeAlongList);
        notifyItemChanged(index);
    }

    public void poiDetailsUpdate(final List<RouteParam> gasChargeAlongList) {
        mGasChargeAlongList.clear();
        mGasChargeAlongList.addAll(gasChargeAlongList);
        notifyDataSetChanged();
    }

    /**
     * 清空列表
     */
    public void clearList() {
        final int size = mPoiEntities.size();
        mPoiEntities.clear();
        if (size > 0) {
            notifyItemRangeRemoved(0, size);
        }
    }

    /**
     * 刷新列表
     * @param searchResultEntity 源数据
     */
    public void notifyList(final SearchResultEntity searchResultEntity) {
        this.mSearchResultEntity = searchResultEntity;
        final List<PoiInfoEntity> newPoiList = searchResultEntity.getPoiList();
        final int oldSize = mPoiEntities.size();
        final int newSize = newPoiList.size();

        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (allPoiParamList.size() >= 2) {
            allPoiParamList.remove(0);
            allPoiParamList.remove(allPoiParamList.size() - 1);
        }

        mPoiEntities.clear();
        mPoiEntities.addAll(newPoiList);
        isFirstRefresh = true;
        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final SearchResultItemBinding adapterSearchResultItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.search_result_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding, this);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        holder.mResultItemBinding.setPoiBean(mPoiEntities.get(position));
        mPoiInfoEntity = mPoiEntities.get(position);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onBindViewHolder searchType: "
                + mSearchResultEntity.getSearchType() + " ,mHomeCompanyType: " + mHomeCompanyType);
        if (mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH
                || mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            holder.mResultItemBinding.poiNum.setVisibility(VISIBLE);
            holder.mResultItemBinding.poiIcon.setVisibility(GONE);
            holder.mResultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        } else {
            holder.mResultItemBinding.poiNum.setVisibility(GONE);
            holder.mResultItemBinding.poiIcon.setVisibility(VISIBLE);
        }
        if (mPoiInfoEntity != null && ConvertUtils.isEmpty(mPoiInfoEntity.getAddress())) {
            holder.mResultItemBinding.subLineView.setVisibility(GONE);
        }
        if(mPoiInfoEntity != null && (mPoiInfoEntity.isClosest() || mPoiInfoEntity.isFastest())){
            holder.mResultItemBinding.searchLabel.setVisibility(VISIBLE);
            holder.mResultItemBinding.searchLabel.setText(mPoiInfoEntity.isClosest() ? R.string.search_label_closest : R.string.search_label_fastest);
        }else{
            holder.mResultItemBinding.searchLabel.setVisibility(GONE);
        }

        //预搜索高亮搜索文本
        if (mSearchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            holder.mResultItemBinding.poiName.setText(matcherSearchTitle(AppCache.getInstance().
                    getMContext().getResources().getColor(R.color.navi_color_006CFF_100),
                    mPoiEntities.get(position).getName(), mSearchResultEntity.getKeyword()));
        }
        if (mSearchPackage.isAlongWaySearch() && !mIsEnd) {
            holder.mResultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add));

            if (isBelongSamePoi(mGasChargeAlongList, mPoiEntities.get(position))) {
                holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                        ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added));
            } else {
                holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_add);
                holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                        ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
            }
            if(!ConvertUtils.isEmpty(mPoiInfoEntity.getMLableList())){
                String labelName = "";
                ArrayList<String> labelList = new ArrayList<>();
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"size: "+mPoiInfoEntity.getMLableList().size());
                for (int i = 0; i < mPoiInfoEntity.getMLableList().size(); i++) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"type: "+mPoiInfoEntity.getMLableList().get(i).getMType());
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"content: "+mPoiInfoEntity.getMLableList().get(i).getMContent());
                    String content = mPoiInfoEntity.getMLableList().get(i).getMContent();
                    // 仅展示其中的四种标签:最近，最顺路，反向，需下高速
                    if(ResourceUtils.Companion.getInstance().getString(R.string.label_nearest).equals(content)
                            || ResourceUtils.Companion.getInstance().getString(R.string.label_optimal).equals(content)
                            || ResourceUtils.Companion.getInstance().getString(R.string.label_opposite).equals(content)
                            || ResourceUtils.Companion.getInstance().getString(R.string.label_exit_highway).equals(content)){
                        labelList.add(content);
                    }
                }
                if(!ConvertUtils.isEmpty(labelList)){
                    labelName = getPriorityString(labelList);
                    holder.mResultItemBinding.searchLabel.setVisibility(VISIBLE);
                    holder.mResultItemBinding.searchLabel.setText(labelName);
                }else{
                    holder.mResultItemBinding.searchLabel.setVisibility(GONE);
                }

            }
        } else {
            holder.mResultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.mResultItemBinding.ivNaviIcon.setImageDrawable(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

        }
        if (mHomeCompanyType != -1) {
            holder.mResultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
            switch (mHomeCompanyType) {
                case 0:
                    if (!BehaviorPackage.getInstance().isFavorite(mPoiInfoEntity).isEmpty()) {
                        holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.mResultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                    }else{
                        holder.mResultItemBinding.textNavi.setText(R.string.st_collect_add);
                    }
                    break;
                case 3:
                    if(BehaviorPackage.getInstance().isFrequentAddress(mPoiInfoEntity)){
                        holder.mResultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.mResultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                    }else{
                        holder.mResultItemBinding.textNavi.setText(R.string.st_collect_add);
                    }
                    break;
                case 1:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_home);
                    break;
                case 2:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_company);
                    break;
                default:
                    holder.mResultItemBinding.textNavi.setText(R.string.st_go_here);
                    break;
            }
        }
        if (mPoiInfoEntity != null) {
            if ("0m".equals(mPoiInfoEntity.getDistance())) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mPoiInfoEntity.getDistance() is: " + mSearchPackage.calcStraightDistance(mPoiInfoEntity.getPoint()));
                mPoiInfoEntity.setDistance(mSearchPackage.calcStraightDistance(mPoiInfoEntity.getPoint()));
            }
            if (mPoiInfoEntity.getPointTypeCode() == null) {
                refreshNormalView(holder);
                return;
            }
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"mLabelName: "+mLabelNameList.size());
            if(!ConvertUtils.isEmpty(mLabelNameList)){
                holder.mResultItemBinding.poiChargeLabel.setVisibility(VISIBLE);
                mSearchResultFilterLabelAdapter = new SearchResultFilterLabelAdapter();
                holder.mResultItemBinding.poiChargeLabel.setAdapter(mSearchResultFilterLabelAdapter);
                mSearchResultFilterLabelAdapter.setLabelNameList(mLabelNameList);
            }else{
                holder.mResultItemBinding.poiChargeLabel.setVisibility(GONE);
            }
            final int pointTypeCode = mSearchPackage.getPointTypeCode(mPoiInfoEntity.getPointTypeCode());
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "pointTypeCode is: " + pointTypeCode);
            switch (pointTypeCode) {
                case AutoMapConstant.PointTypeCode.GAS_STATION:
                    refreshGasStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                    refreshChargeStationView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CAR_WASH:
                    refreshCarWashView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.CATERING:
                    refreshCateringView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.PARKING_LOT:
                    refreshParkingLotView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                    refreshServiceAreaView(holder);
                    break;
                case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                case AutoMapConstant.PointTypeCode.OTHERS:
                default:
                    refreshScenicSpotView(holder);
                    break;
            }
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_LIST_SELECT)
    private static void sendBuryPointForSearchResult(int position, String name) {
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, Integer.toString(position + 1))
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * 刷新通用视图
     * @param resultHolder holder
     */
    private void refreshNormalView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshNormalView do nothing");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
    }

    /**
     * 刷新景点视图
     * @param resultHolder holder
     */
    private void refreshScenicSpotView(final ResultHolder resultHolder) {
        final int pointTypeCode = mSearchPackage.getPointTypeCode(mPoiInfoEntity.getPointTypeCode());
        ScenePoiItemScenicSpotViewBinding binding = resultHolder.getScenicSpotViewBinding();
        if (binding != null) {
            if (pointTypeCode == AutoMapConstant.PointTypeCode.OTHERS) {
                binding.poiScenicSpotPrice.setVisibility(GONE);
                binding.poiScenicSpotPriceIcon.setVisibility(GONE);
                if (ConvertUtils.isEmpty(mPoiInfoEntity) || ConvertUtils.isEmpty(mPoiInfoEntity.getChildInfoList())) {
                    resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
                } else {
                    resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
                }
            } else {
                binding.poiScenicSpotPrice.setVisibility(VISIBLE);
                binding.poiScenicSpotPriceIcon.setVisibility(VISIBLE);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            }
            if (mPoiInfoEntity.getAverageCost() == -1 || mPoiInfoEntity.getAverageCost() == 0) {
                binding.poiScenicSpotPrice.setVisibility(GONE);
                binding.poiScenicSpotPriceIcon.setVisibility(GONE);
            } else {
                binding.poiScenicSpotPrice.setText(String.valueOf(mPoiInfoEntity.getAverageCost()));
            }

            final List<ChildInfo> childInfoList = mPoiInfoEntity.getChildInfoList();
            final PoiListDetailsScenicChildAdapter scenicChildAdapter = new PoiListDetailsScenicChildAdapter(pointTypeCode);
            if (childInfoList != null && !childInfoList.isEmpty()) {
                for (ChildInfo childInfo : childInfoList) {
                    SearchPackage.getInstance().setGrandChildInfoList(childInfo)
                            .thenAccept(childInfoNew -> {
                                if (!ConvertUtils.isEmpty(childInfoNew.getMGrandChildInfoList())) {
                                    mPoiInfoEntity.setMChildType(AutoMapConstant.ChildType.HAS_CHILD_HAS_GRAND);
                                }
                                childInfo.setMGrandChildInfoList(childInfoNew.getMGrandChildInfoList());
                                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChildList " + childInfo.getMGrandChildInfoList());
                            })
                            .exceptionally(error -> {
                                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setGrandChildInfoList error:" + error);
                                return null;
                            });
                }
                scenicChildAdapter.setChildInfoList(childInfoList);
                binding.poiScenicSpotChildList.setVisibility(VISIBLE);
                binding.poiScenicSpotChildList.setLayoutManager(
                        new GridLayoutManager(resultHolder.mResultItemBinding.getRoot().getContext(), mSpanCount));
                if (binding.poiScenicSpotChildList.getItemDecorationCount() == 0) {
                    binding.poiScenicSpotChildList.addItemDecoration(
                            new GridSpacingItemDecoration(resultHolder.mResultItemBinding.getRoot().getContext(), mSpanCount, mSpacing, mSpacing, false));
                }
                binding.poiScenicSpotChildList.setAdapter(scenicChildAdapter);
                scenicChildAdapter.setItemClickListener((index, isSelectIndex) -> {
                    mChildSelectIndex = index;
                    if (isFirstRefresh) {
                        mLastParentSelectIndex = resultHolder.getAdapterPosition();
                        isFirstRefresh = false;
                    }
                    mParentSelectIndex = resultHolder.getAdapterPosition();
                    if (mLastParentSelectIndex != mParentSelectIndex) {
                        if (!ConvertUtils.isEmpty(mPoiEntities) && mLastParentSelectIndex < mPoiEntities.size()) {
                            final PoiInfoEntity poiInfoEntity = mPoiEntities.get(mLastParentSelectIndex);
                            if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(poiInfoEntity.getChildInfoList())) {
                                for (ChildInfo childInfo : poiInfoEntity.getChildInfoList()) {
                                    childInfo.setChecked(0);
                                }
                            }
                            notifyItemChanged(mLastParentSelectIndex);
                        }
                    }
                    if (isSelectIndex) {
                        final ChildInfo childInfo = childInfoList.get(index);
                        mChildSelectInfo = new PoiInfoEntity()
                                .setName(childInfo.getName())
                                .setAddress(childInfo.getAddress())
                                .setPid(childInfo.getPoiId())
                                .setPoint(childInfo.getLocation());
                        if (ConvertUtils.isEmpty(childInfo.getMGrandChildInfoList())) {
                            mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_NO_GRAND);
                        } else {
                            mChildSelectInfo.setMChildType(AutoMapConstant.ChildType.CHILD_HAS_GRAND);
                        }
                    } else {
                        mChildSelectInfo = null;
                    }
                    mLastParentSelectIndex = resultHolder.getAdapterPosition();
                });
            } else {
                binding.poiScenicSpotChildList.setVisibility(GONE);
                binding.poiScenicSpotChildList.setAdapter(null);
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshScenicSpotView3：");
            }
            binding.getRoot().setVisibility(VISIBLE);
        }
    }

    /**
     * 刷新服务区视图
     * @param resultHolder holder
     */
    private void refreshServiceAreaView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshServiceAreaView");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
    }

    /***
     * 判断当前点是否已经在路线上
     * @param local 路径上的点
     * @param poiInfoEntity 当前点
     * @return 是否是属于路径上的点
     */
    private boolean isBelongSamePoi(final List<RouteParam> local, final PoiInfoEntity poiInfoEntity) {
        if (local.isEmpty()) {
            return false;
        }
        for (RouteParam param : local) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /***
     * 从添加的点中删除poi点
     * @param local 路径上的点
     * @param poiInfoEntity 当前点
     */
    private void removeSamePoi(final List<RouteParam> local, final PoiInfoEntity poiInfoEntity) {
        if (local.isEmpty()) {
            return;
        }
        for (RouteParam param : local) {
            if (RoutePackage.getInstance().isTheSamePoi(param, poiInfoEntity)) {
                local.remove(param);
                break;
            }
        }
    }

    /**
     * 刷新停车场视图
     * @param resultHolder holder
     */
    private void refreshParkingLotView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshParkingLotView");
        if (mPoiInfoEntity == null || mPoiInfoEntity.getParkingInfoList() == null || mPoiInfoEntity.getParkingInfoList().isEmpty()) {
            return;
        }
        View parkView = resultHolder.itemView.findViewById(R.id.scene_poi_item_parking_service_view);
        ScenePoiItemParkingViewBinding binding = null;
        if (parkView == null) {
            ViewStub stub = resultHolder.itemView.findViewById(R.id.scene_poi_item_parking_service_view_stub);
            if (stub != null) {
                parkView = stub.inflate(); // 加载 GasStationView
                binding = DataBindingUtil.bind(parkView);
            }
        }
        if (binding != null) {
            binding.poiParkingFreeTotal.setText("");
            binding.getRoot().setVisibility(VISIBLE);
            final ParkingInfo parkingInfo = mPoiInfoEntity.getParkingInfoList().get(0);
            String parkString = "";
            final int spaceFree = parkingInfo.getSpaceFree();
            final int spaceTotal = parkingInfo.getSpaceTotal();
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "spaceFree :" + spaceFree + " spaceTotal :" + spaceTotal);
            // 停车位总数为0，直接隐藏显示
            if ((spaceFree == -1 && spaceTotal == -1) || spaceTotal == 0) {
                binding.getRoot().setVisibility(GONE);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
            } else if (spaceFree == -1) {
                parkString = resultHolder.mResultItemBinding.getRoot().getContext().getString(R.string.parking_lot_total, spaceTotal);
                binding.getRoot().setVisibility(VISIBLE);
                binding.poiParkingFreeTotal.setText(parkString);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            } else {
                binding.getRoot().setVisibility(VISIBLE);
                parkString = resultHolder.mResultItemBinding.getRoot().getContext().getString(R.string.parking_lot_status, spaceFree, spaceTotal);
                binding.poiParkingFreeTotal.setText(parkString);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            }
        }
    }

    /**
     * 刷新餐饮视图
     * @param resultHolder holder
     */
    private void refreshCateringView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshCateringView" + mPoiInfoEntity.getAverageCost());
        ScenePoiItemCateringViewBinding binding = resultHolder.getCateringViewBinding();
        if (binding != null) {
            binding.getRoot().setVisibility(VISIBLE);
            binding.poiCateringPrice.setText("");
            if (mPoiInfoEntity.getAverageCost() == -1) {
                binding.getRoot().setVisibility(GONE);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
            } else {
                binding.poiCateringPrice.setText(String.valueOf(mPoiInfoEntity.getAverageCost()));
                binding.getRoot().setVisibility(VISIBLE);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            }
        }
    }

    /**
     * 刷新洗车视图
     * @param resultHolder holder
     */
    private void refreshCarWashView(final ResultHolder resultHolder) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "refreshCarWashView");
        resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
    }

    /**
     * 刷新充电站视图
     * @param resultHolder holder
     */
    private void refreshChargeStationView(final ResultHolder resultHolder) {
        ScenePoiItemChargeViewBinding binding = resultHolder.getChargeViewBinding();
        if (binding != null) {
            resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
            // 重置includeview中视图状态避免数据重用导致数据异常加载
            binding.poiChargeFastRoot.setVisibility(VISIBLE);
            binding.poiChargeSlowRoot.setVisibility(VISIBLE);
            binding.poiChargeFastFree.setText("");
            binding.poiChargeFastFree.setVisibility(VISIBLE);
            binding.poiChargeFastTotal.setText("");
            binding.poiChargeFastTotal.setVisibility(VISIBLE);
            binding.poiChargeSlowFree.setText("");
            binding.poiChargeSlowFree.setVisibility(VISIBLE);
            binding.poiChargeSlowTotal.setText("");
            binding.poiChargeSlowTotal.setVisibility(VISIBLE);
            final List<ChargeInfo> chargeInfos = mPoiInfoEntity.getChargeInfoList();
            if (ConvertUtils.isEmpty(chargeInfos)) {
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
                return;
            } else {
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            }
            final ChargeInfo chargeInfo = chargeInfos.get(0);
            final String fastFree = chargeInfo.getFast_free() == 0 ? "" : chargeInfo.getFast_free() + "";
            String fastTotal = chargeInfo.getFast_total() == 0 ? "" : "/" + chargeInfo.getFast_total();
            binding.poiChargeFastFree.setText(fastFree);
            if (ConvertUtils.isEmpty(fastFree) && ConvertUtils.isEmpty(fastTotal)) {
                binding.poiChargeFastRoot.setVisibility(GONE);
            } else if (ConvertUtils.isEmpty(fastFree)) {
                binding.poiChargeFastFree.setVisibility(GONE);
            } else if (ConvertUtils.isEmpty(fastTotal)) {
                binding.poiChargeFastTotal.setVisibility(GONE);
            }
            if (!ConvertUtils.isEmpty(fastTotal)) {
                if (ConvertUtils.isEmpty(fastFree)) {
                    fastTotal = chargeInfo.getFast_total() + "";
                }
                binding.poiChargeFastTotal.setText(fastTotal);
            }

            final String slowFree = chargeInfo.getSlow_free() == 0 ? "" : chargeInfo.getSlow_free() + "";
            String slowTotal = chargeInfo.getSlow_total() == 0 ? "" : "/" + chargeInfo.getSlow_total();
            binding.poiChargeSlowFree.setText(slowFree);
            if (ConvertUtils.isEmpty(slowFree) && ConvertUtils.isEmpty(slowTotal)) {
                binding.poiChargeSlowRoot.setVisibility(GONE);
            } else if (ConvertUtils.isEmpty(slowFree)) {
                binding.poiChargeSlowFree.setVisibility(GONE);
            } else if (ConvertUtils.isEmpty(slowTotal)) {
                binding.poiChargeSlowTotal.setVisibility(GONE);
            }
            if (!ConvertUtils.isEmpty(slowTotal)) {
                if (ConvertUtils.isEmpty(slowFree)) {
                    slowTotal = chargeInfo.getSlow_total() + "";
                }
                binding.poiChargeSlowTotal.setText(slowTotal);
            }
            if (!ConvertUtils.equals(chargeInfo.getCurrentElePrice(), "--")) {
                binding.poiChargePrice.setText(
                        resultHolder.mResultItemBinding.getRoot().getContext().getString(
                                R.string.charge_price_simple, chargeInfo.getCurrentElePrice()));
                binding.poiChargePrice.setVisibility(VISIBLE);
                binding.poiCarChargePriceIcon.setVisibility(VISIBLE);
            } else {
                binding.poiChargePrice.setVisibility(GONE);
                binding.poiCarChargePriceIcon.setVisibility(GONE);
            }
            binding.getRoot().setVisibility(VISIBLE);
        }
    }

    /**
     * 刷新加油站视图
     * @param resultHolder holder
     */
    private void refreshGasStationView(final ResultHolder resultHolder) {
        ScenePoiItemGasViewBinding binding = resultHolder.getGasViewBinding();
        if (binding != null) {
            final List<GasStationInfo> gasStationInfos = mPoiInfoEntity.getStationList();
            for (GasStationInfo gasStationInfo : gasStationInfos) {
                if (!gasStationInfo.getPrice().contains("升")) {
                    gasStationInfo.setPrice(resultHolder.mResultItemBinding.getRoot().getContext().
                            getString(R.string.oil_price, gasStationInfo.getPrice()));
                }
            }
            final GasStationAdapter gasStationAdapter = new GasStationAdapter();
            //根据UE只显示前两个油品种类
            if (!ConvertUtils.isEmpty(gasStationInfos) && gasStationInfos.size() > 2) {
                // 截取前两个元素
                final List<GasStationInfo> subList = gasStationInfos.subList(0, 2);
                // 使用ArrayList构造函数创建新的列表
                final ArrayList<GasStationInfo> newList = new ArrayList<>(subList);
                gasStationAdapter.setGasStationList(newList);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            } else if (!ConvertUtils.isEmpty(gasStationInfos) && gasStationInfos.size() <= 2) {
                gasStationAdapter.setGasStationList(gasStationInfos);
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(VISIBLE);
            } else {
                resultHolder.mResultItemBinding.crlPoiDetail.setVisibility(GONE);
            }
            binding.poiGasOilList.setLayoutManager(new GridLayoutManager(resultHolder.itemView.getRootView().getContext(), mSpanCount));
            binding.poiGasOilList.setAdapter(gasStationAdapter);

        }
    }


    @Override
    public int getItemCount() {
        return mPoiEntities.size();
    }


    /**
     * 清除途径点列表
     */
    public void clearViaList() {
        notifyDataSetChanged();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        private final SearchResultItemBinding mResultItemBinding;
        private final WeakReference<SearchResultAdapter> mAdapterReference;
        // 缓存 ScenePoiItemScenicSpotViewBinding
        private ScenePoiItemScenicSpotViewBinding scenicSpotViewBinding;
        private ScenePoiItemChargeViewBinding chargeViewBinding;
        private ScenePoiItemCateringViewBinding cateringViewBinding;
        private ScenePoiItemGasViewBinding gasViewBinding;

        /**
         * 获取 scenicSpotViewBinding，如果未初始化则 inflate 并绑定
         */
        public ScenePoiItemScenicSpotViewBinding getScenicSpotViewBinding() {
            if (scenicSpotViewBinding == null) {
                ViewStub stub = itemView.findViewById(R.id.scene_poi_item_scenic_spot_view_stub);
                if (stub != null) {
                    View view = stub.inflate(); // 加载 ViewStub
                    scenicSpotViewBinding = DataBindingUtil.bind(view); // 绑定
                }
            }
            return scenicSpotViewBinding;
        }

        /**
         * 获取 ScenePoiItemChargeViewBinding，如果未初始化则 inflate 并绑定
         */
        public ScenePoiItemCateringViewBinding getCateringViewBinding() {
            if (cateringViewBinding == null) {
                ViewStub stub = itemView.findViewById(R.id.scene_poi_item_catering_view_stub);
                if (stub != null) {
                    View view = stub.inflate(); // 加载 ViewStub
                    cateringViewBinding = DataBindingUtil.bind(view); // 绑定
                }
            }
            return cateringViewBinding;
        }

        /**
         * 获取 ScenePoiItemChargeViewBinding，如果未初始化则 inflate 并绑定
         */
        public ScenePoiItemChargeViewBinding getChargeViewBinding() {
            if (scenicSpotViewBinding == null) {
                ViewStub stub = itemView.findViewById(R.id.scene_poi_item_charge_view_stub);
                if (stub != null) {
                    View view = stub.inflate(); // 加载 ViewStub
                    chargeViewBinding = DataBindingUtil.bind(view); // 绑定
                }
            }
            return chargeViewBinding;
        }

        /**
         * 获取 ScenePoiItemChargeViewBinding，如果未初始化则 inflate 并绑定
         */
        public ScenePoiItemGasViewBinding getGasViewBinding() {
            if (gasViewBinding == null) {
                ViewStub stub = itemView.findViewById(R.id.scene_poi_item_gas_view_stub);
                if (stub != null) {
                    View view = stub.inflate(); // 加载 ViewStub
                    gasViewBinding = DataBindingUtil.bind(view); // 绑定
                }
            }
            return gasViewBinding;
        }
        public ResultHolder(final SearchResultItemBinding resultItemBinding,
                            final SearchResultAdapter adapter) {
            super(resultItemBinding.getRoot());
            this.mResultItemBinding = resultItemBinding;
            this.mResultItemBinding.setHolder(this);
            this.mAdapterReference = new WeakReference<>(adapter);

            final ResultHolder holder = resultItemBinding.getHolder();
            final RecyclerView.ItemDecoration quickItemDecoration = new HorizontalSpaceItemDecoration(4);
            holder.mResultItemBinding.poiChargeLabel.setLayoutManager(new LinearLayoutManager(holder.mResultItemBinding.getRoot().getContext(), RecyclerView.HORIZONTAL, false));
            holder.mResultItemBinding.poiChargeLabel.addItemDecoration(quickItemDecoration);


            resultItemBinding.getRoot().setOnClickListener(v -> {
                int position = getAdapterPosition();
                if (position == RecyclerView.NO_POSITION) {
                    return;
                }

                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 详情");
                SearchResultAdapter adapterRef = mAdapterReference.get();
                if (adapterRef != null && adapterRef.mOnItemClickListener != null) {
                    PoiInfoEntity entity = adapterRef.mPoiEntities.get(position);
                    if (entity != null) {
                        sendBuryPointForSearchResult(position, entity.getName());
                        if (adapterRef.mParentSelectIndex == position && adapterRef.mChildSelectIndex != -1) {
                            adapterRef.mOnItemClickListener.onChildItemClick(position, entity, adapterRef.mChildSelectIndex);
                        } else {
                            adapterRef.mOnItemClickListener.onItemClick(position, entity);
                        }
                    }
                }
            });

            resultItemBinding.poiToNavi.setOnClickListener(v -> {
                int position = getAdapterPosition();
                if (position == RecyclerView.NO_POSITION) {
                    return;
                }
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 算路|添加途经点");
                SearchResultAdapter adapterRef = mAdapterReference.get();
                if (adapterRef != null && adapterRef.mOnItemClickListener != null) {
                    if (adapterRef.mParentSelectIndex == position && adapterRef.mChildSelectIndex != -1 && adapterRef.mChildSelectInfo != null) {
                        adapterRef.mOnItemClickListener.onNaviClick(position, adapterRef.mChildSelectInfo);
                    } else {
                        PoiInfoEntity entity = adapterRef.mPoiEntities.get(position);
                        if (entity != null) {
                            adapterRef.mOnItemClickListener.onNaviClick(position, entity);
                        }
                    }
                }
            });
        }
    }

    /**
     * 高亮显示文本
     *
     * @param color   需要高亮显示的颜色
     * @param text    显示的文字
     * @param keyword 搜索关键词
     * @return SpannableString
     */
    private SpannableString matcherSearchTitle(final int color, final String text, final String keyword) {
        final SpannableString spannableString = new SpannableString(text);
        if (null != keyword && !TextUtils.isEmpty(keyword)) {
            try {
                final Pattern pattern = Pattern.compile(keyword);
                final Matcher matcher = pattern.matcher(spannableString);
                while (matcher.find()) {
                    final int start = matcher.start();
                    final int end = matcher.end();
                    spannableString.setSpan(new ForegroundColorSpan(color), start, end, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
                }
            } catch (PatternSyntaxException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Invalid regex pattern: " + e.getMessage());
            } catch (IndexOutOfBoundsException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Index out of bounds: " + e.getMessage());
            } catch (IllegalStateException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Matcher state error: " + e.getMessage());
            } catch (IllegalArgumentException e) {
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "Argument error: " + e.getMessage());
            }
        }
        return spannableString;
    }

    /**
     * 设置快筛label
     * @param labelNameList
     */
    public void setQuickLabel(ArrayList<String> labelNameList){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"QuickLabel: "+labelNameList.size());
        mLabelNameList.clear();
        mLabelNameList.addAll(labelNameList);
        notifyDataSetChanged();
    }

    public String getPriorityString(ArrayList<String> tags) {
        List<String> priorityOrder = Arrays.asList(
                ResourceUtils.Companion.getInstance().getString(R.string.label_optimal),
                ResourceUtils.Companion.getInstance().getString(R.string.label_nearest),
                ResourceUtils.Companion.getInstance().getString(R.string.label_opposite),
                ResourceUtils.Companion.getInstance().getString(R.string.label_exit_highway)
        );
        for (String priority : priorityOrder) {
            if (tags.contains(priority)) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"priority: "+priority);
                // 映射：需调头-》反向、最近-》最快速
                if(ResourceUtils.Companion.getInstance().getString(R.string.label_opposite).equals(priority)){
                    return ResourceUtils.Companion.getInstance().getString(R.string.label_opposite_rv);
                }else if(ResourceUtils.Companion.getInstance().getString(R.string.label_nearest).equals(priority)){
                    return ResourceUtils.Companion.getInstance().getString(R.string.label_nearest_rv);
                }else{
                    return priority;
                }
            }
        }
        return "";
    }

    private boolean isSamePoi(final PoiInfoEntity local, final PoiInfoEntity poiInfoEntity) {
        if (local == null || poiInfoEntity == null || local.getPoint() == null || poiInfoEntity.getPoint() == null) {
            return false;
        }
        return (ConvertUtils.equals(local.getPid(), poiInfoEntity.getPid()))
                || (local.getPoint().getLat() == poiInfoEntity.getPoint().getLat()
                && local.getPoint().getLon() == poiInfoEntity.getPoint().getLon());
    }

    public int getTargetIndex(final PoiInfoEntity poiInfoEntity) {
        if (ConvertUtils.isEmpty(mPoiEntities)) {
            return -1;
        }
        for (int i = 0; i < mPoiEntities.size(); i++) {
            if (isSamePoi(mPoiEntities.get(i), poiInfoEntity)) {
                return i;
            }
        }
        return -1;
    }

    public interface OnItemClickListener {
        /**
         * 点击事件
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         */
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         * 导航按钮点击事件
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         */
        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);

        /**
         *
         * @param position 点击下标
         * @param poiInfoEntity 选中的数据
         * @param childPosition 选中的子点下标
         */
        default void onChildItemClick(int position, PoiInfoEntity poiInfoEntity, int childPosition){

        }
    }
}