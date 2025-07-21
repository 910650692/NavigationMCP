package com.sgm.navi.scene.ui.adapter;

import static com.sgm.navi.service.define.navi.HandCardType.PARK;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.ItemHandingCardDetailBinding;
import com.sgm.navi.scene.ui.navi.hangingcard.CardManager;
import com.sgm.navi.scene.ui.navi.hangingcard.OnHandingCardItemClickListener;
import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.ParkingInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/19
 * Description: [在这里描述文件功能]
 */
public class HandingCardDetailAdapter extends RecyclerView.Adapter<HandingCardDetailAdapter.MyViewHolder> {

    private static final String TAG = "HandingCardDetailAdapter";
    private List<PoiInfoEntity> dataList = new ArrayList<>();
    private HandCardType mType = HandCardType.CHARGE;
    private Context mContext;
    private OnHandingCardItemClickListener mItemClickListener;
    private int mSelectIndex;

    public HandingCardDetailAdapter(Context context, OnHandingCardItemClickListener listener) {
        this.mContext = context;
        this.mItemClickListener = listener;
    }

    public void notifyDataChanged(List<PoiInfoEntity> infoEntities, HandCardType type) {
        dataList.clear();
        dataList.addAll(infoEntities);
        mType = type;
        mSelectIndex = 0;
        notifyDataSetChanged();
        ThreadManager.getInstance().postDelay(() -> {
            if (!ConvertUtils.isEmpty(dataList) && !ConvertUtils.isNull(mItemClickListener)) {
                PoiInfoEntity poiInfo = dataList.get(0);
                int searchType = AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH;
                if (mType == PARK) {
                    searchType = AutoMapConstant.SearchType.AROUND_SEARCH;
                }
                mItemClickListener.onItemSelect(0, poiInfo, searchType);
            }
        }, 200);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        final ItemHandingCardDetailBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(mContext), R.layout.item_handing_card_detail, parent, false);
        return new MyViewHolder(itemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        switch (mType) {
            case CHARGE -> setChargeUi(holder, position);
            case GAS -> setGasUi(holder, position);
            case PARK -> setParkUi(holder, position);
            default -> {
            }
        }

        final ItemHandingCardDetailBinding binding = holder.binding;
        if (binding == null){
            Logger.d(TAG, "MyViewHolder.binding == null");
            return;
        }
        binding.clCharge.itemRoot.setVisibility(mType == HandCardType.CHARGE ? View.VISIBLE : View.GONE);
        binding.clGas.itemRoot.setVisibility(mType == HandCardType.GAS ? View.VISIBLE : View.GONE);
        binding.clPark.sclListItem.setVisibility(mType == PARK ? View.VISIBLE : View.GONE);
        binding.clCharge.itemRoot.setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        binding.clGas.itemRoot.setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        binding.clPark.sclListItem.setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
    }

    private void setChargeUi(MyViewHolder holder, int position) {
        final ItemHandingCardDetailBinding binding = holder.binding;
        if (binding == null){
            Logger.d(TAG, "MyViewHolder.binding == null");
            return;
        }
        binding.clCharge.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        final PoiInfoEntity poiInfo = dataList.get(position);
        final ChargeInfo chargeInfo = ConvertUtils.isEmpty(poiInfo.getChargeInfoList()) ? null : poiInfo.getChargeInfoList().get(0);
        binding.clCharge.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        binding.clCharge.itemRoot.setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        binding.clCharge.stvNum.setText(String.valueOf(position + 1));
        binding.clCharge.tvTitle.setText(poiInfo.getName());
        binding.clCharge.tvSubTitle.setVisibility(CardManager.getInstance().judgePoiIsService(poiInfo) ? View.VISIBLE : View.GONE);
        if (!ConvertUtils.isNull(chargeInfo)) {
            binding.clCharge.llQuick.setVisibility(chargeInfo.getFast_total() > 0 ? View.VISIBLE : View.GONE);
            binding.clCharge.llSlow.setVisibility(chargeInfo.getSlow_total() > 0 ? View.VISIBLE : View.GONE);
            if (chargeInfo.getFast_total() + chargeInfo.getSlow_total() > 0) {
                if (CardManager.getInstance().judgePoiIsService(poiInfo) && chargeInfo.getFast_total() > 0 && chargeInfo.getSlow_total() > 0) {
                    binding.clCharge.tvSum.setVisibility(View.VISIBLE);
                    binding.clCharge.tvSum2.setVisibility(View.GONE);
                } else {
                    binding.clCharge.tvSum.setVisibility(View.GONE);
                    binding.clCharge.tvSum2.setVisibility(View.VISIBLE);
                }
            } else {
                binding.clCharge.tvSum.setVisibility(View.GONE);
                binding.clCharge.tvSum2.setVisibility(View.GONE);
            }
            binding.clCharge.tvQuickFree.setText(String.valueOf(chargeInfo.getFast_free()));
            binding.clCharge.tvQuickTotal.setText("/" + chargeInfo.getFast_total());
            binding.clCharge.tvSlowFree.setText(String.valueOf(chargeInfo.getSlow_free()));
            binding.clCharge.tvSlowTotal.setText("/" + chargeInfo.getSlow_total());
            binding.clCharge.tvDistance.setText(poiInfo.getDistance());
            final int desc = CardManager.getInstance().isPlentiful(chargeInfo) ? R.string.yong_ji : R.string.chong_zu;
            binding.clCharge.tvSum.setText(desc);
            binding.clCharge.tvSum2.setText(desc);
        } else {
            binding.clCharge.llQuick.setVisibility(View.GONE);
            binding.clCharge.llSlow.setVisibility(View.GONE);
            binding.clCharge.tvSum.setVisibility(View.GONE);
            binding.clCharge.tvSum2.setVisibility(View.GONE);
        }
        binding.getRoot().setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                if (position != mSelectIndex) {
                    mSelectIndex = position;
                    notifyDataSetChanged();
                }
                mItemClickListener.onItemSelect(mSelectIndex, poiInfo, AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH);
            }
        });
        binding.clCharge.viewNaviNow.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                mItemClickListener.onNaviNow(position);
            }
        });
    }

    private void setGasUi(MyViewHolder holder, int position) {
        final ItemHandingCardDetailBinding binding = holder.binding;
        final PoiInfoEntity poiInfo = dataList.get(position);

        if (binding == null){
            Logger.d(TAG, "MyViewHolder.binding == null");
            return;
        }
        binding.clGas.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        binding.clGas.stvNum.setText(String.valueOf(position + 1));
        binding.clGas.tvTitle.setText(poiInfo.getName());
        binding.clGas.tvSubTitle.setVisibility(CardManager.getInstance().judgePoiIsService(poiInfo) ? View.VISIBLE : View.GONE);
        binding.clGas.tvRecLabel.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        binding.getRoot().setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
        if (!ConvertUtils.isNull(poiInfo)) {
            binding.clGas.tvDistance.setText(poiInfo.getDistance());
        }
        final List<GasStationInfo> infos = poiInfo.getStationList();
        if (ConvertUtils.isEmpty(infos)) {
            binding.clGas.ll95.setVisibility(View.GONE);
            binding.clGas.ll92.setVisibility(View.GONE);
        } else if (infos.size() >= 2) {
            binding.clGas.ll95.setVisibility(View.VISIBLE);
            binding.clGas.ll92.setVisibility(View.VISIBLE);
            final GasStationInfo firstBean = infos.get(0);
            final GasStationInfo secondBean = infos.get(1);
            binding.clGas.tv95.setVisibility(View.VISIBLE);
            binding.clGas.tv95.setText(firstBean.getType());
            binding.clGas.tv95Price.setText(String.format(mContext.getString(R.string.price_sdf), firstBean.getPrice()));

            binding.clGas.tv92.setVisibility(View.VISIBLE);
            binding.clGas.tv92.setText(secondBean.getType());
            binding.clGas.tv92Price.setText(String.format(mContext.getString(R.string.price_sdf), secondBean.getPrice()));
        } else if (infos.size() == 1) {
            binding.clGas.ll95.setVisibility(View.VISIBLE);
            binding.clGas.ll92.setVisibility(View.VISIBLE);

            final GasStationInfo firstBean = infos.get(0);
            binding.clGas.tv95.setVisibility(View.VISIBLE);
            binding.clGas.tv95.setText(firstBean.getType());
            binding.clGas.tv95Price.setText(String.format(mContext.getString(R.string.price_sdf), firstBean.getPrice()));

            binding.clGas.tv92.setVisibility(View.GONE);
            binding.clGas.tv92Price.setVisibility(View.GONE);
        }
        binding.getRoot().setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                if (position != mSelectIndex) {
                    mSelectIndex = position;
                    notifyDataSetChanged();
                }
                mItemClickListener.onItemSelect(mSelectIndex, poiInfo, AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH);
            }
        });
        binding.clGas.viewNaviNow.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mItemClickListener)) {
                mItemClickListener.onNaviNow(position);
            }
        });
    }

    @SuppressLint("SetTextI18n")
    private void setParkUi(MyViewHolder holder, int position) {
        final ItemHandingCardDetailBinding binding = holder.binding;
        if (binding == null){
            Logger.d(TAG, "MyViewHolder.binding == null");
            return;
        }
        binding.clPark.tvRecommend.setVisibility(position == 0 ? View.VISIBLE : View.GONE);
        final PoiInfoEntity poiInfo = dataList.get(position);
        final List<ParkingInfo> parkingInfos = poiInfo.getParkingInfoList();
        if (ConvertUtils.isEmpty(parkingInfos)) {
            return;
        }
        final ParkingInfo parkingInfo = parkingInfos.get(0);
        binding.clPark.tvNumber.setText(String.valueOf(position + 1));
        binding.clPark.tvTitle.setText(poiInfo.getName());
        binding.clPark.tvDistance.setText(poiInfo.getDistance());
        binding.clPark.sclListItem.setOnClickListener(v -> {
            if (mItemClickListener != null && mSelectIndex != position) {
                mItemClickListener.onItemSelect(position, poiInfo, AutoMapConstant.SearchType.AROUND_SEARCH);
                final int tIndex = mSelectIndex;
                mSelectIndex = position;
                notifyItemChanged(tIndex);
                notifyItemChanged(mSelectIndex);

                // for bury point
                sendBuryPointForSelectingParkingPot(position);
            }
        });
        binding.clPark.viewNaviNow.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onNaviNow(position);
            }
        });
        if (parkingInfo != null) {
            binding.clPark.tvSpace.setVisibility(parkingInfo.getSpaceFree() > 0 ? View.VISIBLE : View.GONE);
            binding.clPark.tvTotal.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
            binding.clPark.tvDesc.setVisibility((parkingInfo.getSpaceTotal() > 0 && parkingInfo.getSpaceFree() > 0) ? View.VISIBLE : View.GONE);
            binding.clPark.tvSpace.setText(String.valueOf(parkingInfo.getSpaceFree()));
            binding.clPark.tvTotal.setText((parkingInfo.getSpaceFree() > 0 ? "/" : "") + parkingInfo.getSpaceTotal());
            final String desc = CardManager.getInstance().parkIsCrowed(poiInfo) ? binding.clPark.tvDesc.getContext().getString(R.string.tense)
                    : binding.clPark.tvDesc.getContext().getString(R.string.chong_zu);
            binding.clPark.tvDesc.setText(desc);
        } else {
            binding.clPark.tvSpace.setVisibility(View.GONE);
            binding.clPark.tvTotal.setVisibility(View.GONE);
            binding.clPark.tvDesc.setVisibility(View.GONE);
        }
        binding.clPark.tvEnd.setVisibility(poiInfo.getIsEndPoint() ? View.VISIBLE : View.GONE);
        binding.clPark.sclListItem.setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
    }

    @Override
    public int getItemCount() {
        return dataList.size();
    }

    public int getSelectIndex() {
        return mSelectIndex;
    }


    public static final class MyViewHolder extends RecyclerView.ViewHolder {
        public ItemHandingCardDetailBinding binding;

        public MyViewHolder(@NonNull ItemHandingCardDetailBinding itemBinding) {
            super(itemBinding.getRoot());
            this.binding = itemBinding;
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_PARKING_SELECT)
    private void sendBuryPointForSelectingParkingPot(int position) {
        String props = "第" + (position + 1) + "个";
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, props)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }
}
