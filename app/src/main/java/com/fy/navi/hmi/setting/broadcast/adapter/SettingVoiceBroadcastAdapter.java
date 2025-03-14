package com.fy.navi.hmi.setting.broadcast.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemRecommenedVoiceBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class SettingVoiceBroadcastAdapter extends RecyclerView.Adapter<SettingVoiceBroadcastAdapter.Holder>{

    private static final String TAG = "SettingVoiceBroadcastAdapter";

    private List<VoiceInfo> mVoiceInfoList;
    private OnItemClickListener mOnItemClickListener;


    @SuppressLint("NotifyDataSetChanged")
    public void setData(HashMap<Integer, VoiceInfo> voiceInfoList) {
        mVoiceInfoList = new ArrayList<>(voiceInfoList.values());
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void setSingleChoice(int index) {
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            mVoiceInfoList.get(i).setUsed(i == index);
        }
        notifyDataSetChanged();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void unSelectAllVoices() {
        for (int i = 0; i < mVoiceInfoList.size(); i++) {
            mVoiceInfoList.get(i).setUsed(false);
        }
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener mOnItemClickListener) {
        this.mOnItemClickListener = mOnItemClickListener;
    }

    @NonNull
    @Override
    public SettingVoiceBroadcastAdapter.Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemRecommenedVoiceBinding itemRecommenedVoiceBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.item_recommened_voice, parent, false);
        return new Holder(itemRecommenedVoiceBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull SettingVoiceBroadcastAdapter.Holder holder, int position) {
        holder.voiceBroadcastBinding.recommendVoiceProgress.setProgressDrawable(AppContext.mApplication.getDrawable(R.drawable.progress_bar_style));
        holder.voiceBroadcastBinding.setModel(mVoiceInfoList.get(position));

        holder.voiceBroadcastBinding.recommendVoiceOperate.setOnClickListener(v -> {
            if (mOnItemClickListener != null) {
                mOnItemClickListener.onOperation(position);
            }
        });
    }

    @Override
    public int getItemCount() {
        if(mVoiceInfoList == null){
            return 0;
        }
        return mVoiceInfoList.size();
    }

    public static class Holder extends RecyclerView.ViewHolder{
        public ItemRecommenedVoiceBinding voiceBroadcastBinding;
        public Holder(ItemRecommenedVoiceBinding voiceBroadcastBinding) {
            super(voiceBroadcastBinding.getRoot());
            this.voiceBroadcastBinding = voiceBroadcastBinding;
            voiceBroadcastBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener{
        void onOperation(int index);
    }
}
