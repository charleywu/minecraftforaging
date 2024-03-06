package com.arc.forage.gamemaster.events;

import com.arc.forage.blocks.RewardBlock;
import com.arc.forage.gamemaster.GameMaster;
import com.arc.forage.gamemaster.L;
import com.arc.forage.gamemaster.WebAPI;
import com.arc.forage.init.ModBlocks;
import com.arc.forage.settings.ForageSettings;
import com.arc.forage.structures.Field;
import com.arc.forage.tools.FieldTable;
import com.arc.forage.tools.StyledText;

import net.minecraft.block.Block;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;

public class GM_FieldPrep implements GMEvent {

	private int round_index;

	public GM_FieldPrep(int round_index) {
		this.round_index = round_index;
	}

	@Override
	public boolean process(int tsle) {
		WebAPI.add_info("Preparing round...");
		
		BlockPos field_origin = new BlockPos(ForageSettings.get_int("/structures/field/origin/x"), ForageSettings.get_int("/structures/field/origin/y"), ForageSettings.get_int("/structures/field/origin/z"));
		Vec3i field_offset = new Vec3i(ForageSettings.get_int("/structures/field/offset/x"), ForageSettings.get_int("/structures/field/offset/y"), ForageSettings.get_int("/structures/field/offset/z"));
		int field_interval = ForageSettings.get_int("/game/settings/field_interval");
		String enviro_path = "environments/" + ForageSettings.get_string("/game/roundData/"+round_index+"/envFile");
		boolean separate = ForageSettings.get_bool("/game/roundData/"+round_index+"/soloRound");
		boolean training = ForageSettings.get_bool("/game/roundData/"+round_index+"/trainingRound");
		String field_type = ForageSettings.get_string("/game/roundData/"+round_index+"/blockType");

		StyledText nxt_rnd_brd = new StyledText(L.get_s("you_will_now_play"));
		if(training) nxt_rnd_brd.append(L.get_s("training"), "gray", "bold");
		else if(separate) nxt_rnd_brd.append(L.get_s("separate"), "dark_aqua", "bold");
		else nxt_rnd_brd.append(L.get_s("social"), "light_purple", "bold");
		nxt_rnd_brd.append(L.get_s("on_a"));
		if(field_type.contentEquals("melon")) nxt_rnd_brd.append(L.get_s("melonfield"), "dark_green", "bold");
		else nxt_rnd_brd.append(L.get_s("pumpkinfield"), "gold", "bold");
		GameMaster.broadcast_styled(nxt_rnd_brd);

		FieldTable table = new FieldTable(enviro_path);
		Block b;
		if(field_type.equals("melon")) b = ModBlocks.magicMelon;
		else b = ModBlocks.magicPumpkin;

		if(separate) {
			for(int i=0; i<GameMaster.player_amount; i++) {
				Field f = new Field(GameMaster.server.getEntityWorld(), field_origin.add(new Vec3i(i*field_offset.getX(), i*field_offset.getY(), i*field_offset.getZ())), table, field_interval, b);
				GameMaster.addField(f);
			}
		}
		else {
			Field f = new Field(GameMaster.server.getEntityWorld(), field_origin, table, field_interval, b);
			GameMaster.addField(f);
		}
		
		WebAPI.add_info("Preparations complete.");

		return true;
	}

	@Override
	public GMEType get_type() {
		return GMEType.FIELDPREP;
	}

	@Override
	public String get_status() {
		return "Preparing Field...";
	}

}
